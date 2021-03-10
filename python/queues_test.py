from time import sleep
import asyncio
import queue
import janus
import random

random.seed()

startAsync = lambda coro: asyncio.create_task(coro)

async def waitAsync(coro):
    task = startAsync(coro)
    return await task

startSync = lambda func, *args, **kwargs: asyncio.get_running_loop().run_in_executor(None, func, *args, **kwargs)

async def waitSync(func, *args, **kwargs):
    future = startSync(func, *args, **kwargs)
    return await future

async def async_task(read_q, write_q, mon_q):
    print('async_task started!')
    n = 0
    while True:
        msg = (await read_q.get()) + str(n)
        if msg is None: break
        print('async_task:', n, msg)
        await mon_q.put(msg)
        await write_q.put(msg)
        n += 1
        if n > 10:
            await mon_q.put(None)
            await write_q.put(None)
            break
        await asyncio.sleep(random.randint(0, n))
    return 'async_task ended!'

async def async_monitor(qu):
    print('async_monitor started!')
    n = 0
    while True:
        msg = await qu.get()
        if msg is None: break
        print('async_monitor:', n, msg)
        n += 1
    return 'async_monitor ended!'

def sync_task(read_q, write_q, mon_q):
    print('sync_task started!')
    n = 0
    while True:
        msg = read_q.get() + str(n)
        if msg is None: break
        print('sync_task:', n, msg)
        mon_q.put(msg)
        write_q.put(msg)
        n += 1
        if n > 10:
            mon_q.put(None)
            write_q.put(None)
            break
        sleep(random.randint(0, n))
    return 'sync_task ended!'

def sync_monitor(qu):
    print('sync_monitor started!')
    n = 0
    while True:
        msg = qu.get()
        if msg is None: break
        print('sync_monitor:', n, msg)
        n += 1
    return 'sync_monitor ended!'

async def main():
    async_mon_q = asyncio.Queue()
    task1 = startAsync(async_monitor(async_mon_q))

    sync_mon_q = queue.Queue()
    future1 = startSync(sync_monitor, sync_mon_q)

    q_async_to_sync = janus.Queue()
    q_sync_to_async = janus.Queue()

    task2 = startAsync(async_task(q_sync_to_async.async_q, q_async_to_sync.async_q, async_mon_q))
    future2 = startSync(sync_task, q_async_to_sync.sync_q, q_sync_to_async.sync_q, sync_mon_q)

    q_async_to_sync.sync_q.put('to-sync')
    await q_sync_to_async.async_q.put('to-async')

    r = await asyncio.gather(task1, task2, future1, future2)
    print(r)

    q_async_to_sync.close()
    q_sync_to_async.close()

    await q_async_to_sync.wait_closed()
    await q_sync_to_async.wait_closed()

asyncio.run(main())

