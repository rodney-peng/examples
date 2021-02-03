static void filterAndDo( unsigned int (* filter)( fd_set * ),
                         void (* action)( unsigned int, unsigned int ),
                         unsigned int data )
{
    fd_set  fdSet;
    unsigned int  i;

    Opsem_waitApi( &regSem );

    if (filter( &fdSet ))
    {
        for (i = 1; i <= GROUP_TERM_LINE_FLAG + MAX_HUNTGROUP_NUMBER; i++)
        {
            if (FD_ISSET(i, &fdSet))
            {
                action( i, data );
            }
        }
    }

    Opsem_postApi( &regSem );
}

static unsigned int findRegisteredLines( fd_set * pFdSet )
{
	tLineObj *pObj;
    unsigned int n = 0;

    FD_ZERO(pFdSet);

	Oppthread_mutex_lockApi(&(sg_mutex));

    // all lines and hunt groups

	pObj = peekDLListFront(sg_dll);

	while (pObj)
	{
        if (lineTermGetRegState(pObj->pLine) == REG_STATE_REGISTERED)
        {
            FD_SET( lineTermGetIdx(pObj->pLine), pFdSet);
            n++;
        }
#if 0
        else if (lineTermGetRegState(pObj->pLine) == REG_STATE_REGISTERING)
        {
            printf( "%s(%u): line %u reging\n", __func__, __LINE__, lineTermGetIdx(pObj->pLine) );

            FD_SET( lineTermGetIdx(pObj->pLine), pFdSet);
            n++;
        }
#endif

		pObj = getDLListItemNext(pObj);
	}

	Oppthread_mutex_unlockApi(&(sg_mutex));

    return n;
}

static unsigned int findNotRegisteredLines( fd_set * pFdSet )
{
	tLineObj *pObj;
    unsigned int n = 0;

    FD_ZERO(pFdSet);

	Oppthread_mutex_lockApi(&(sg_mutex));

    // all lines and hunt groups

	pObj = peekDLListFront(sg_dll);

	while (pObj)
	{
        // Don't check by lineTermIsEnReg( pObj->pLine ) because SIP instance needs to be updated by
        // SipProtoCfgSipInsApi() in reg_Event_Handler() even registration is disabled

        if (lineTermIsActivated( pObj->pLine ) &&
            (lineTermGetRegState(pObj->pLine) != REG_STATE_REGISTERED))
        {
            if (lineTermGetRegState(pObj->pLine) != REG_STATE_REGISTERING)
            {
                FD_SET( lineTermGetIdx(pObj->pLine), pFdSet);
                n++;
            }
#if 0
            else
                printf( "%s(%u): line %u reging\n", __func__, __LINE__, lineTermGetIdx(pObj->pLine) );
#endif
        }

		pObj = getDLListItemNext(pObj);
	}

	Oppthread_mutex_unlockApi(&(sg_mutex));

    return n;
}

#define UNREGISTER( lineIdx ) \
{ \
    if (EventSendUnRegisterApi( lineIdx, OpFalse, OpTrue ) < 0) /* CC event */ \
        printf( "%s(%u): line %u EventSendUnRegisterApi() failed\n", __func__, __LINE__, lineIdx ); \
}

static void _unregister( unsigned int lineIdx, unsigned int data )
{
    UNREGISTER( lineIdx );
}

// notify call control to unregister all lines if currently registered
void LineMgrUnregisterAll( void )
{
    filterAndDo( findRegisteredLines, _unregister, 0 );
}

#define REGISTER( lineIdx ) \
{   /* If line registration is disabled, this is only to update SIP instance and no REGISTER will be sent. */ \
    if (EventSendRegisterApi( lineIdx, OpFalse, OpFalse ) < 0) /* SIP event */ \
        printf( "%s(%u): line %u EventSendRegisterApi() failed\n", __func__, __LINE__, lineIdx ); \
}

static void _register( unsigned int lineIdx, unsigned int data )
{
    REGISTER( lineIdx );
}

// register all lines if not currently registered
void LineMgrRegisterAll( void )
{
    filterAndDo( findNotRegisteredLines, _register, 0 );
}

static void unregisterIfUseThisServer( unsigned int lineIdx, unsigned int data )
{
    tLineTerm  *pLine = lineMgrGetTermByIdx( lineIdx );
    eGLOBAL_SERVER  server = (eGLOBAL_SERVER)data;

    if (lineTermGetGlobalRegServer( pLine ) == server)
        UNREGISTER( lineIdx );
}

// unregister lines if currently registered to the global server
void LineMgrUnregisterFromServer( eGLOBAL_SERVER globalServer )
{
    filterAndDo( findRegisteredLines, unregisterIfUseThisServer, globalServer );
}

static void registerIfUseGlobalServer( unsigned int lineIdx, unsigned int data )
{
    tLineTerm  *pLine = lineMgrGetTermByIdx( lineIdx );

    // re-select global server
    if (lineTermSetGlobalRegServer( pLine ) != GLOBAL_SERVER_UNKNOWN)
        REGISTER( lineIdx );
}

// register lines use global server and not currently registered
void LineMgrRegisterToGlobalServer( void )
{
    filterAndDo( findNotRegisteredLines, registerIfUseGlobalServer, 0 );
}

static void registerIfNotUseGlobalServer( unsigned int lineIdx, unsigned int data )
{
    tLineTerm  *pLine = lineMgrGetTermByIdx( lineIdx );

    if (! lineTermUseGlobalRegServer( pLine ))
        REGISTER( lineIdx );
}

// register lines don't use global server and not currently registered
void LineMgrRegisterToProfileServer( void )
{
    filterAndDo( findNotRegisteredLines, registerIfNotUseGlobalServer, 0 );
}

static void registerIfUseThisServer( unsigned int lineIdx, unsigned int data )
{
    tLineTerm  *pLine = lineMgrGetTermByIdx( lineIdx );
    eGLOBAL_SERVER  server = (eGLOBAL_SERVER)data;

    if (lineTermUseGlobalRegServer( pLine ))
    {
        // if this line uses global server, register it to this server

        lineTermSpecifyGlobalRegServer( pLine, server );
        REGISTER( lineIdx );
    }
    else if (strcmp( CfgMgrGlobalRegistrar( server ), lineTermGetRegistrar( pLine ) ) == 0)
    {
        // if this line doesn't use global server but it is configured to the same global server, register it

        REGISTER( lineIdx );
    }
}

// register lines use global server or are configured to the same server in the profile
void LineMgrRegisterToServer( eGLOBAL_SERVER globalServer )
{
    filterAndDo( findNotRegisteredLines, registerIfUseThisServer, globalServer );
}

