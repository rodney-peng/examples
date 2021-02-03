/*
    break and continue in a do { } while (0) block for exception handling

*/

void * ProvUpgradeFirmware(void *args)
{
	tUpgradeFirmware  *pCfg = (tUpgradeFirmware *)args;
	char  cmd[512];
    struct stat  fileStat;
    int  status = 0;

    do
    {
        if ((status = stat( pCfg->sFileName, &fileStat )) != 0)
        {
	        upgradeState  = UPGRADE_STATE_ERROR;
            upgradeStatus = UPGRADE_ERROR_INVALID_FILE;

            break;
        }

        fileSize = fileStat.st_size;
        upgradeState = UPGRADE_STATE_EXTRACT;

        sprintf( cmd, "extract '%s' "PROV_REL_NOTES, pCfg->sFileName );
        if ((status = run_jffs2_script( eJST_UPGRADE, cmd )) != 0)
        {
		    upgradeState  = UPGRADE_STATE_ERROR;
            upgradeStatus = UPGRADE_ERROR_INVALID_FILE;

	        unlink( pCfg->sFileName );

            break;
        }

        upgradeState = UPGRADE_STATE_PROGRESS;

        Oppthread_mutex_lockApi(&upgMutex);
        Oppthread_cond_signalApi(&upgCond);
        Oppthread_mutex_unlockApi(&upgMutex);

	  if (run_jffs2_script( eJST_UPGRADE, "upgrade" ) != 0)
    {
        upgradeState  = UPGRADE_STATE_ERROR;
        upgradeStatus = UPGRADE_ERROR_FAILED;

        break;
    }

        upgradeState = UPGRADE_STATE_COMPLETE;

    } while (0);

    unlink( PROV_REL_NOTES );

    if (status != 0)
    {
        Oppthread_mutex_lockApi(&upgMutex);
        Oppthread_cond_signalApi(&upgCond);
        Oppthread_mutex_unlockApi(&upgMutex);
    }

	provUpgradeThread = -1;

	Oppthread_exitApi(NULL);
	return NULL;
}

