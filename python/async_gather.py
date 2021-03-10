import asyncio
from time import sleep

startAsync = lambda coro: asyncio.create_task(coro)

async def waitAsync(coro):
    task = startAsync(coro)
    return await task

startSync = lambda func, *args, **kwargs: asyncio.get_running_loop().run_in_executor(None, func, *args, **kwargs)

async def waitSync(func, *args, **kwargs):
    future = startSync(func, *args, **kwargs)
    return await future


# call asyncio.gather() start task and return _GatheringFuture
# but call gather() below returns coroutine object
async def gather(*coros, return_exceptions=False):
    tasks = []
    result = []
    for coro in coros:
        task = asyncio.create_task(coro)    # create and start task
        tasks.append(task)
    error = None
    for task in tasks:
        if error:
            task.cancel();
            continue
        try:
            r = await task  # wait result
            print(f'OK => {task}')
            result.append(r)
        except Exception as e:
            print(f'ERROR => {task}')
            if return_exceptions:
                result.append(e)
            else:
                error = e
    if error: raise error
    return result

async def factorial(name, number):
    f = 1
    for i in range(2, number + 1):
        print(f"Task {name}: Compute factorial({i})...")
        await asyncio.sleep(1)
        f *= i
    print(f"Task {name}: factorial({number}) = {f}")
    if f == 120: raise Exception('120')
    return f

async def task(name, number):
    print(f'task {name} {number} begin!')
    asg = asyncio.gather(
        factorial(name+'-1', number),
        factorial(name+'-2', number+1),
        factorial(name+'-3', number+2), return_exceptions=True
    )
    print(f'{asg=}')
    result = await asg
    print(f'task {name} {number} done! {result}')
    return result

async def async_sleep(name, n):
    print(f'{name} go to sleep', n)
    await asyncio.sleep(n)
    print(f'{name} wake up')
    return name, n

def goto_sleep(name, n):
    print(f'{name} go to sleep', n)
    sleep(n)
    print(f'{name} wake up')
    return name, n

def compare_objects(**kw_objs):
    objs = kw_objs.values()
    names = kw_objs.keys()
    n = len(kw_objs)
    dirs = [sorted(dir(obj)) for obj in objs]
    lens = [len(d) for d in dirs]
    width = max(max(len(s) for s in d) for d in dirs)
    print(f'{n=} {width=}')

    def printRow(cols, width, delim=''):
        for col in cols:
            col += delim
            print(f'{col:{width+1}}', end='')
        else:
            print()

    printRow(names, width, ',')
    printRow(['-'*(width+1)]*n, width)

    idx = [0]*n
    while True:
        items = [dirs[i][idx[i]] if idx[i] < lens[i] else chr(128) for i in range(n)]
        cur = min(items)
        for i in range(n):
            if items[i] == cur:
                idx[i] += 1
            else:
                items[i] = ''
        printRow(items, width, ',')
        if all(list(idx[i] >= lens[i] for i in range(n))): break

async def main():
    # Schedule three calls *concurrently*:
    coro = gather(
        task("A", 2),
        task("B", 3),
        task("C", 4),
    )
    print(f'{coro=}')
    cog = startAsync(coro)

    r = startAsync(async_sleep("Async-1", 5))
    print(r)
    r = startSync(goto_sleep, 'Sync-1', 3)
    print(r)
    await asyncio.sleep(2)

    print(f'{cog=}')
    r = await cog
    print(f'{await cog=}')
#    compare_objects(coroutine=coro, task=cog, future=asyncio.get_running_loop().create_future())

asyncio.run(main())

