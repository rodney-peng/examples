#include "../inc/tool_api.h"
#include "../inc/cli.h"

#define _MEMORY_UNIT_MAGIC_NUMBER 0x8034
#define _MEMORY_BEGIN_BORDER_SIZE 4
#define _MEMORY_BEGIN_BORDER_VALUE 0x55
#define _MEMORY_END_BORDER_SIZE 4
#define _MEMORY_END_BORDER_VALUE 0xAA

typedef struct _MemoryUnit {
    OpU16 magicNumber;
    OpS8 *pFnStr;
    OpU32 line;
    OpStimeval allocTime;
    OpU32 size;
    struct list_head head;
    OpS8 begin[_MEMORY_BEGIN_BORDER_SIZE];
    OpS8 space;
    OpS8 end[_MEMORY_END_BORDER_SIZE];
} tMemoryUnit;

typedef struct
{
    unsigned long address;
    unsigned long size;
    char          type;
    const char *  name;

} SYMBOL_TABLE;

extern SYMBOL_TABLE __attribute__((weak)) SymbolTable[];
static int symbolTableFixed = 0;

static void * main = NULL;
static void * osThreadWrapper_t = NULL;

static OpBool memoryLeakTraceEnable;
static struct list_head memoryDLL;
static Oppthread_mutex_t memoryMutex;
static struct CmdDef cmdToolShowAllocMemory;
static struct CmdDef cmdToolMemoryLeakTrace;

static struct CmdDef cmdMemTraceShowThreads;
static struct CmdDef cmdMemTraceAddPID;
static struct CmdDef cmdMemTraceBegin;
static struct CmdDef cmdMemTraceShow;
static struct CmdDef cmdShowThread;

int fixSymbolTable( SYMBOL_TABLE * symbolTable );

extern void (* pthread_create_hook)( void * entry, void * arg );
extern void (* malloc_hook)( size_t size, void * ptr );
extern void (* free_hook)( size_t size, void * ptr );

static inline void changeIRQ(unsigned int NewState);

static void my_pthread_create_hook( void * entry, void * arg );
static void my_malloc_hook( size_t size, void * ptr );
static void my_free_hook( size_t size, void * ptr );

static OpS32 _CmdMemTraceShowThreads(OpS32 argc, OpS8 *argv[]);
static OpS32 _CmdMemTraceAddPID(OpS32 argc, OpS8 *argv[]);
static OpS32 _CmdMemTraceBegin(OpS32 argc, OpS8 *argv[]);
static OpS32 _CmdMemTraceShow(OpS32 argc, OpS8 *argv[]);

void _ShowAllocMemoryHelp(void)
{
    printf("%s SEC\n", cmdToolShowAllocMemory.name);
}

static OpS32 _ShowAllocMemoryFun(OpS32 argc, OpS8 *argv[])
{
    tMemoryUnit *pMUnit = NULL;
    OpS32 sec = 0;
    OpU32 passSec, number = 0, size = 0;
    OpStimeval nowTime;

    if (argc == 2)
        sec = atoi(argv[1]);

    Oppthread_mutex_lockApi(&memoryMutex);

    OpgettimeofdayApi(&nowTime, NULL);

    list_for_each_entry(pMUnit, &memoryDLL, head) {
        passSec = nowTime.tv_sec - pMUnit->allocTime.tv_sec;
        if (passSec > sec) {
            printf("f:%s l:%u t:%u.%u s:%u\n", pMUnit->pFnStr, pMUnit->line, pMUnit->allocTime.tv_sec,
                pMUnit->allocTime.tv_usec, pMUnit->size);
            number++;
            size += pMUnit->size;
        }
    }

    Oppthread_mutex_unlockApi(&memoryMutex);

    printf("number: %u size: %u\n", number, ((sizeof (tMemoryUnit))-1)*number+size);

    return 0;
}

void _MemoryLeakTraceHelp(void)
{
    printf("%s CFG\n", cmdToolMemoryLeakTrace.name);
    printf("\tCFG:\n"
        "\t\t0: Disable\n"
        "\t\t1: Enable\n");
}

OpS32 ToolMemoryLeakTraceSwitchApi(OpBool cfg)
{
    Oppthread_mutex_lockApi(&memoryMutex);
    memoryLeakTraceEnable = cfg;
    Oppthread_mutex_unlockApi(&memoryMutex);

    return 0;
}

static OpS32 _MemoryLeakTraceFun(OpS32 argc, OpS8 *argv[])
{
    OpS32 rv;
    OpBool cfg;

    if (argc != 2)
        goto show_help;

    if (strcmp(argv[1], "1") == 0)
        rv = ToolMemoryLeakTraceSwitchApi(OpTrue);
    else
        rv = ToolMemoryLeakTraceSwitchApi(OpFalse);

    MCEReturn(rv < 0, "ToolMemoryLeakTraceSwitchApi() fail:%d", rv);

    return 0;

show_help:
    _MemoryLeakTraceHelp();
    return 0;
}

OpS32 ToolMemModuleInitApi(void)
{
    changeIRQ(0);
    malloc_hook = my_malloc_hook;
    free_hook   = my_free_hook;
    changeIRQ(1);

    INIT_LIST_HEAD(&memoryDLL);
    Oppthread_mutex_initApi(&memoryMutex, NULL);
    memoryLeakTraceEnable = OpFalse;

    symbolTableFixed = fixSymbolTable( SymbolTable );

    changeIRQ(0);
    pthread_create_hook = my_pthread_create_hook;
    changeIRQ(1);

    if (main) pthread_create_hook( main, NULL );

    return 0;
}

OpS32 ToolMemRegCmdApi(void)
{
    CliDefCmdApi(cmdToolShowAllocMemory, "tsma", "Tool Show Alloc Memory",
        OpTrue, _ShowAllocMemoryFun, _ShowAllocMemoryHelp);
    assert(CliRegCmdApi(&cmdToolShowAllocMemory) >= 0);

    CliDefCmdApi(cmdToolMemoryLeakTrace, "tmlt", "Tool Memory Leak Trace",
        OpTrue, _MemoryLeakTraceFun, _MemoryLeakTraceHelp);
    assert(CliRegCmdApi(&cmdToolMemoryLeakTrace) >= 0);

    CliDefCmdApi(cmdMemTraceShowThreads, "mtst", "memory trace: show threads", OpFalse,
        _CmdMemTraceShowThreads, NULL);
    assert(CliRegCmdApi(&cmdMemTraceShowThreads) >= 0);

    CliDefCmdApi(cmdMemTraceAddPID, "mtap", "memory trace: add PID", OpFalse,
        _CmdMemTraceAddPID, NULL);
    assert(CliRegCmdApi(&cmdMemTraceAddPID) >= 0);

    CliDefCmdApi(cmdMemTraceBegin, "mtb", "memory trace: begin", OpFalse,
        _CmdMemTraceBegin, NULL);
    assert(CliRegCmdApi(&cmdMemTraceBegin) >= 0);

    CliDefCmdApi(cmdMemTraceShow, "mts", "memory trace: show statistics", OpFalse,
        _CmdMemTraceShow, NULL);
    assert(CliRegCmdApi(&cmdMemTraceShow) >= 0);

    return 0;
}

#ifdef malloc
#undef malloc
#endif

#ifdef free
#undef free
#endif

#ifdef realloc
#undef realloc
#endif

#ifdef strdup
#undef strudp
#endif

void* ToolMemAllocApi(OpU32 size, OpS8 *pFile, OpU32 line)
{ /* For debug memory leak and detect over&under flow */
    tMemoryUnit *pMUnit = NULL;

    MCEGoto(pFile == NULL, fail, "pFile == NULL");
    MCEGoto(size == 0, fail, "size == %d", size);

    pMUnit = (tMemoryUnit *)malloc(sizeof(tMemoryUnit) + size - 1);

    MCEGoto(pMUnit == NULL, fail, "OpmallocApi() fail");

    pMUnit->magicNumber = _MEMORY_UNIT_MAGIC_NUMBER;

    pMUnit->pFnStr = pFile;
    pMUnit->line = line;

    OpgettimeofdayApi(&pMUnit->allocTime, NULL);

    INIT_LIST_HEAD(&pMUnit->head);

    pMUnit->size = size;

    memset(pMUnit->begin, _MEMORY_BEGIN_BORDER_VALUE, _MEMORY_BEGIN_BORDER_SIZE);

    memset(&pMUnit->space, 0, size);

    memset((&pMUnit->space) + size, _MEMORY_END_BORDER_VALUE,
        _MEMORY_END_BORDER_SIZE);

    if (memoryLeakTraceEnable == OpTrue) {
        Oppthread_mutex_lockApi(&memoryMutex);
        list_add_tail(&pMUnit->head, &memoryDLL);
        Oppthread_mutex_unlockApi(&memoryMutex);
    }

    return (void *)&pMUnit->space;

free_pMUnit:
    free(pMUnit);
fail:
    return NULL;
}

void ToolMemFreeApi(void *pPtr, OpS8 *pFile, OpU32 line)
{
    tMemoryUnit *pMUTmp = NULL, *pMUnit = NULL;
    OpS8 *pTmp = NULL;
    OpS32 i;

    MCEExit(pFile == NULL, "pFile:%s line:%d pFile == NULL", pFile, line);
    MCEExit(pPtr == NULL, "pFile:%s line:%d pPtr == NULL", pFile, line);

    if (memoryLeakTraceEnable == OpTrue) {
        Oppthread_mutex_lockApi(&memoryMutex);
        list_for_each_entry(pMUTmp, &memoryDLL, head) {
            if (&pMUTmp->space == pPtr) {
                pMUnit = pMUTmp;
                break;
            }
        }
        Oppthread_mutex_unlockApi(&memoryMutex);
    }

    if (pMUnit == NULL)
        pMUnit = container_of(pPtr, tMemoryUnit, space);

    if (pMUnit->magicNumber != _MEMORY_UNIT_MAGIC_NUMBER) {
        //ELog("pFile:%s line:%d", pFile, line);
        free(pPtr);
        pPtr = NULL;
        return;
    }

    pTmp = pMUnit->begin;
    for (i = 0; i < _MEMORY_BEGIN_BORDER_SIZE; i++) {
        if (pTmp[i] != _MEMORY_BEGIN_BORDER_VALUE) {
            ToolHexDumpApi(pMUnit->begin, _MEMORY_BEGIN_BORDER_SIZE +
                pMUnit->size + _MEMORY_END_BORDER_SIZE,
                _MEMORY_BEGIN_BORDER_SIZE + pMUnit->size +
                _MEMORY_END_BORDER_SIZE, "Begin");
            MCEGoto(OpTrue, stop, "pTmp:%p pPtr:%p pFnStr:%s allocTime:%u.%u "
                "size:%u underflow detect", pTmp, pPtr, pMUnit->pFnStr,
                pMUnit->allocTime.tv_sec, pMUnit->allocTime.tv_usec,
                pMUnit->size);

        }
    }

    pTmp = (&pMUnit->space) + pMUnit->size;
    for (i = 0; i < _MEMORY_END_BORDER_SIZE; i++) {
        if (pTmp[i] & 0xFF != _MEMORY_END_BORDER_VALUE) {
            printf("pTmp[%d]:%x != %x", i, pTmp[i], _MEMORY_END_BORDER_VALUE);
            ToolHexDumpApi(pMUnit->begin, _MEMORY_BEGIN_BORDER_SIZE +
                pMUnit->size + _MEMORY_END_BORDER_SIZE,
                _MEMORY_BEGIN_BORDER_SIZE + pMUnit->size +
                _MEMORY_END_BORDER_SIZE, "End");
            MCEGoto(OpTrue, stop, "pTmp:%p pPtr:%p pFnStr:%s allocTime:%u.%u "
                "size:%u overflow detect", pTmp, pPtr, pMUnit->pFnStr,
                pMUnit->allocTime.tv_sec, pMUnit->allocTime.tv_usec,
                pMUnit->size);
        }
    }

    Oppthread_mutex_lockApi(&memoryMutex);
    list_del_init(&pMUnit->head);
    Oppthread_mutex_unlockApi(&memoryMutex);

    free(pMUnit);

    return;

stop:
    printf("Stop here!!!!\n");
    while(1)
        ToolMSleepApi(100);
    return;
}

void* ToolMemReallocApi(void *pOld, OpU32 size, OpS8 *pFile, OpU32 line)
{ /* For debug memory leak and detect over&under flow */
    tMemoryUnit *pMUTmp = NULL, *pMUnit = NULL, *pNewMUnit = NULL;
    void *pNew = NULL;
    OpS32 rv;

    MCEGoto(pFile == NULL, fail, "pFile == NULL");
    MCEGoto(pOld == NULL, fail, "pOld == NULL");
    MCEGoto(size == 0, fail, "size == %d", size);

    if (pOld == NULL)
        return ToolMemAllocApi(size, pFile, line);

    if (memoryLeakTraceEnable == OpTrue) {
        Oppthread_mutex_lockApi(&memoryMutex);
        list_for_each_entry(pMUTmp, &memoryDLL, head) {
            if (&pMUTmp->space == pOld) {
                pMUnit = pMUTmp;
                break;
            }
        }
        Oppthread_mutex_unlockApi(&memoryMutex);
    }

    if (pMUnit == NULL)
        pMUnit = container_of(pOld, tMemoryUnit, space);

    if (pMUnit->magicNumber != _MEMORY_UNIT_MAGIC_NUMBER) {
        ELog("pFile:%s line:%d", pFile, line);
        pNew = (void *)realloc(pOld, size);
        return pNew;
    }

    if (pMUnit->size >= size)
        return (void *)&pMUnit->space;

    pNew = ToolMemAllocApi(size, pFile, line);
    MCEGoto(pNew == NULL, fail, "ToolMemAlloc() fail:%d", rv);

    pNewMUnit = container_of(pNew, tMemoryUnit, space);

    memcpy(&pNewMUnit->space, &pMUnit->space, pMUnit->size);

    ToolMemFreeApi(&pMUnit->space, __FILE__, __LINE__);

    return (void *)&pNewMUnit->space;

fail:
    return NULL;
}

void *ToolMemStrdupApi(OpS8 *pStr, OpS8 *pFile, OpU32 line)
{
    OpS8 *pNew = NULL;

    tMemoryUnit *pMUnit = NULL;

    MCEGoto(pStr == NULL, fail, "pStr == NULL");
    MCEGoto(pFile == NULL, fail, "pFile == NULL");

    pNew = ToolMemAllocApi(strlen(pStr) + 1, pFile, line);
    MCEGoto(pNew == NULL, fail, "ToolMemAllocApi() fail");

    strncpy(pNew, pStr, strlen(pStr));
    pNew[strlen(pStr)] = '\0';

    return pNew;

fail:
    return NULL;
}

inline void libc_free(void * mem)
{
    free(mem);
}

int fixSymbolTable( SYMBOL_TABLE * symbolTable )
{
    SYMBOL_TABLE * symbol = symbolTable;
    unsigned long  offset = 0xffffffff;

    while (symbol->name && (strcmp( symbol->name, __FUNCTION__ ) != 0))
    {
        symbol++;
    }

    if (symbol->name != NULL)
    {
        int  n = 0;

        offset = (unsigned long)fixSymbolTable - symbol->address;

        symbol = symbolTable;

        while (symbol->name)
        {
            symbol->address += offset;

            if (main == NULL && strcmp( symbol->name, "main" ) == 0)
            {
                main = (void *)symbol->address;
            }

            if (osThreadWrapper_t == NULL && strcmp( symbol->name, "osThreadWrapper_t" ) == 0)
            {
                osThreadWrapper_t = (void *)symbol->address;
            }

            n++;
            symbol++;
        }

        printf( "Total %u symbols, offset %p\n", n, offset );
    }

    return offset != 0xffffffff;
}

SYMBOL_TABLE * lookupSymbolByAddr( void * _addr )
{
    unsigned long   addr = (unsigned long)_addr;
    SYMBOL_TABLE  * symbol = SymbolTable;

    if (! symbolTableFixed) return NULL;

    if (addr < 0xffffffff)
    {
        while ((symbol->address + symbol->size) <= addr)
        {
            symbol++;
        }

        if (symbol->size && symbol->name) return symbol;
    }

    return NULL;
}

SYMBOL_TABLE * lookupSymbolByName( const char * name )
{
    SYMBOL_TABLE  * symbol = SymbolTable;

    if (! symbolTableFixed) return NULL;

    while (symbol->name != NULL && strcmp( name, symbol->name ) != 0)
    {
        symbol++;
    }

    return (symbol->size && symbol->name) ? symbol : NULL;
}

#include <sys/types.h>
#include <unistd.h>

#include "os_thread.h"

typedef struct
{
  int  pid;
  int  ppid;

  char  name[128];

} REGISTER_THREAD;

static int regThreadNum = 0;
static REGISTER_THREAD  regThreads[64];

/* NewState=1 will enable IRQ, NewState=0 will disable IRQ
   ARM core must be in a privileged mode, e.g. supervisor  */

static inline void changeIRQ(unsigned int NewState)
{
  int my_cpsr;

  asm volatile(
    "MRS %[my_cpsr], CPSR \n\t"
    "ORR %[my_cpsr], %[my_cpsr], #0x80 \n\t"
    "BIC %[my_cpsr], %[my_cpsr], %[NewState], LSL #7 \n\t"
    "MSR CPSR_c, %[my_cpsr] \n\t" : [my_cpsr]"+r"(my_cpsr) : [NewState]"r"(NewState) );
}

void my_pthread_create_hook( void * entry, void * arg )
{
    REGISTER_THREAD  * th;
    SYMBOL_TABLE     * symbol;

    changeIRQ(0);
    th = regThreads + (regThreadNum++);

    th->pid = getpid();
    th->ppid = getppid();
    memset( th->name, 0, sizeof (th->name) );

    if (osThreadWrapper_t != NULL && entry == osThreadWrapper_t)
    {
        entry = ((tOsThread *)arg)->func;
        arg   = ((tOsThread *)arg)->data;
    }

    symbol = lookupSymbolByAddr( entry );
    if (symbol)
        strncpy( th->name, symbol->name, (sizeof (th->name)) - 1 );
    else
        sprintf( th->name, "%p", entry );
    changeIRQ(1);

    printf( "Registered thread: ppid %u pid %u %s\n",
        th->ppid,
        th->pid,
        th->name );
}

typedef struct
{
  int  pid;
  unsigned long  allocCount;
  unsigned long  allocSize;
  unsigned long  freeCount;
  unsigned long  freeSize;

} MONITOR_THREAD;

static unsigned long  totalAllocCount = 0;
static unsigned long  totalAllocSize = 0;
static unsigned long  totalFreeCount = 0;
static unsigned long  totalFreeSize = 0;

static unsigned long  untrackAllocCount = 0;
static unsigned long  untrackAllocSize = 0;
static unsigned long  untrackFreeCount = 0;
static unsigned long  untrackFreeSize = 0;

static int memTrace = 0;
static int monThreadsNum = 0;
static MONITOR_THREAD monThreads[32];

void my_malloc_hook( size_t size, void * ptr )
{
    static int c = 0;

    if (memTrace)
    {
        int  pid = getpid();
        int  i;

        totalAllocCount++;
        totalAllocSize += size;

        for (i = 0; i < monThreadsNum; i++)
        {
            if (pid == monThreads[i].pid)
            {
                monThreads[i].allocCount++;
                monThreads[i].allocSize += size;

                return;
            }
        }

        untrackAllocCount++;
        untrackAllocSize += size;
    }
}

void my_free_hook( size_t size, void * ptr )
{
    if (memTrace)
    {
        int  pid = getpid();
        int  i;

        totalFreeCount++;
        totalFreeSize += size;

        for (i = 0; i < monThreadsNum; i++)
        {
            if (pid == monThreads[i].pid)
            {
                monThreads[i].freeCount++;
                monThreads[i].freeSize += size;

                return;
            }
        }

        untrackFreeCount++;
        untrackFreeSize += size;
    }
}

static OpS32 _CmdMemTraceShowThreads(OpS32 argc, OpS8 *argv[])
{
    int nthread;
    int i;

    changeIRQ(0);
    nthread = regThreadNum;
    changeIRQ(1);

    for (i = 0; i < nthread; i++)
    {
        printf( "ppid %3i pid %3i %s\n",
            regThreads[i].ppid,
            regThreads[i].pid,
            regThreads[i].name );
    }

    return 0;
}

static OpS32 _CmdMemTraceAddPID(OpS32 argc, OpS8 *argv[])
{
    if (argc > 1)
    {
        memset( &monThreads[monThreadsNum], 0, sizeof monThreads[0] );
        monThreads[monThreadsNum].pid = atoi( argv[1] );

        printf( "Add PID %i, total %i\n", monThreads[monThreadsNum].pid, monThreadsNum+1 );

        monThreadsNum++;
    }

    return 0;
}

static OpS32 _CmdMemTraceBegin(OpS32 argc, OpS8 *argv[])
{
    #define PS_LIST  "/tmp/mt_pid_list"
    #define PS_NAME  "itg6_sip"

    if (monThreadsNum == 0)
    {
        char * ps_name = PS_NAME;
        char   cmd[256];
        char * buf;
        size_t size;
        FILE * fp;

        if (argc > 1) ps_name = argv[1];

        system( "rm -f "PS_LIST );

        sprintf( cmd, "for i in $(ps | grep %s | grep -v grep | awk '{print $1}'); do echo $i >> "PS_LIST"; done",
            ps_name );
        system( cmd );

        fp = fopen( PS_LIST, "r" );
        if (fp)
        {
            buf  = cmd;
            size = sizeof cmd;

            while (getline( &buf, &size, fp ) > 0)
            {
                memset( &monThreads[monThreadsNum], 0, sizeof monThreads[0] );
                monThreads[monThreadsNum].pid = atoi( buf );

                if (monThreads[monThreadsNum].pid > 0)
                {
                    printf( "Add PID %i, total %i\n", monThreads[monThreadsNum].pid, monThreadsNum+1 );

                    monThreadsNum++;
                }

                buf  = cmd;
                size = sizeof cmd;
            }

            fclose( fp );
        }
        else
        {
            printf( "Unable to open "PS_LIST" !\n" );
        }
    }

    if (monThreadsNum > 0)
    {
        printf( "\nMemory trace begins!\n" );

        memTrace = 1;
    }

    return 0;
}

static OpS32 _CmdMemTraceShow(OpS32 argc, OpS8 *argv[])
{
    int  i;

    for (i = 0; i < monThreadsNum; i++)
    {
        printf( " PID %3i alloc: s %10lu(c %10lu) free: s %10lu(c %10lu)\n",
            monThreads[i].pid,
            monThreads[i].allocSize,
            monThreads[i].allocCount,
            monThreads[i].freeSize,
            monThreads[i].freeCount );
    }

    printf( " Total   alloc: s %10lu(c %10lu) free: s %10lu(c %10lu)\n",
        totalAllocSize,
        totalAllocCount,
        totalFreeSize,
        totalFreeCount );

    printf( " Untrack alloc: s %10lu(c %10lu) free: s %10lu(c %10lu)\n",
        untrackAllocSize,
        untrackAllocCount,
        untrackFreeSize,
        untrackFreeCount );

    return 0;
}
