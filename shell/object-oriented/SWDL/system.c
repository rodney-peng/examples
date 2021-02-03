#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <brg_xml.h>
#include <brg_proc.h>
#include <brg_log.h>
#include <brg_mem.h>
#include <brg_string.h>
#include <mo_register.h>
#include <server.h>
#include "common.h"
#include "rgwmakeconfig.h"
#include <mgmt_db_enum.h>
#include <client_protocol.h>
#include "rgwapi.h"
#include <commit.h>
#include <tecom/inc/config.h>

void getuboot(char *sn, char *pwd, char *mac, char *mode,char *firmware_ver)
{
	FILE *fp;
#ifndef __LILAC__
	char tmp_warever[64]={0};
	int status;
#endif
	//int i;
	system("/sbin/fw_printenv ONTG_SN | cut -d= -f2 > /tmp/sys");
	//system("/sbin/fw_printenv ONTG_PWD | cut -d= -f2 >> /tmp/sys");
	//system("/sbin/fw_printenv macaddr | cut -d= -f2 >> /tmp/sys");
	//system("/sbin/fw_printenv opermode | cut -d= -f2 >> /tmp/sys");
	if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}	
	if(NULL==fgets(sn, 63, fp))
	{
		brg_printf_console("ONTG_SN read failed!\n");
		//goto exit;
	}
	fclose(fp);

	system("/sbin/fw_printenv ONTG_PWD | cut -d= -f2 > /tmp/sys");
	if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}
	if(NULL==fgets(pwd, 63, fp))
	{
		brg_printf_console("ONTG_PWD read failed!\n");
		//goto exit;
	}
	fclose(fp);

	system("/sbin/fw_printenv macaddr | cut -d= -f2 > /tmp/sys");
	if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}
	if(NULL==fgets(mac, 63, fp))
	{
		brg_printf_console("WIFI_MAC read failed!\n");
		//goto exit;
	}
	fclose(fp);

	system("/sbin/fw_printenv opermode | cut -d= -f2 > /tmp/sys");
	if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}
	if(NULL==fgets(mode, 63, fp))
	{
		brg_printf_console("DEV_MODE read failed!\n");
		//goto exit;
	}
	fclose(fp);
#ifdef __LILAC__
    /* TECOM_SWVER_FILE is defined in drivers/tecom/inc/config.h */
    if ((fp = fopen ( TECOM_SWVER_FILE, "r" )) != NULL)
    {
        fgets(firmware_ver, 16, fp);
        fclose(fp);
    }
    else
    {
        strcpy( firmware_ver, "NULL" );
    }
#else
	system("/sbin/fw_printenv SW1_ACTIVE | cut -d= -f2 > /tmp/sys");
	if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}
	if(NULL==fgets(tmp_warever, 63, fp))
	{
		brg_printf_console("tmp_warever read failed!\n");
		//goto exit;
	}
	fclose(fp);

	if(strlen(tmp_warever)>1)
	{
		tmp_warever[strlen(tmp_warever)-1]=0;
	}
	
	brg_system(&status, NULL,"/sbin/fw_printenv SW%s_VER | cut -d= -f2 > /tmp/sys",tmp_warever);
	if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}
	if(NULL==fgets(firmware_ver, 63, fp))
	{
		brg_printf_console("firmware_ver read failed!\n");
		//goto exit;
	}
	fclose(fp);
#endif
	if(strlen(sn)>0)
	{
		sn[strlen(sn)-1]=0;
	}
	if(strlen(pwd)>0)
	{
		pwd[strlen(pwd)-1]=0;
	}
	if(strlen(mac)>0)
	{
		mac[strlen(mac)-1]=0;
	}
	if(strlen(mode)>0)
	{
		mode[strlen(mode)-1]=0;
	}
	return;
}
/*
static void sync2db(char *sn, char*pwd, char *mac)
{
	char ontg_sn[64], ontg_pwd[64], wifi_mac[64];
	xmlNodePtr sys, root = get_mgmt_old_db_root_by_state();

	brg_xml_val_t v_wifi_mactest;
	char *wifi_mactest;
    xmlNodePtr db = get_mgmt_db_root_by_state();
	
	if(NULL==sn||NULL==pwd||NULL==mac)
	{
		return;
	}
	
	getuboot(ontg_sn, ontg_pwd, wifi_mac);

	brg_printf_console("ONTG_SN: %s\n", ontg_sn);
	brg_printf_console("ONTG_PWD: %s\n", ontg_pwd);
	brg_printf_console("WIFI_MAC: %s\n", wifi_mac);

	if((0!=strcmp(ontg_sn, sn))||
		(0!=strcmp(ontg_pwd, pwd))||
		(0!=strcmp(wifi_mac, mac)))
	{
		brg_printf_console("write to db mac[%s]\n",wifi_mac);
		sys= brg_xml_get_path_single_verify(root, "%s", "/system");
		brg_printf_console("sys node: %d\n", (int)sys);
		brg_xml_set_path_str(sys, "/ontg_sn", ontg_sn);
		brg_xml_set_path_str(sys, "/ontg_pwd", ontg_pwd);
		brg_xml_set_path_str(sys, "/wifi_mac", wifi_mac);
		wifi_mactest = brg_xml_get_path_str(v_wifi_mactest, db, "/system/wifi_mac");
		
		brg_printf_console("get from db mac[%s]\n",wifi_mactest);
		server_commit();
	}
	return ;
}	
*/

#define SET_PORT_OPERMODE(status, port, mode) \
        brg_system(&status, NULL, "fw_setenv %s_opermode %s", port, mode);

static void sync2uboot(char *sn,int hex_on, char*pwd, char *mac,char *mode)
{
	char ontg_sn[64]={0}, ontg_pwd[64]={0}, wifi_mac[64]={0},dev_mode[64]={0},firmware_ver[64]={0};
	char cmd[128];
	int status = 0;	
     char *setpwd = (char *) malloc(100);
     int i;
	if(NULL==sn||NULL==pwd||NULL==mac)
	{
		return;
	}
	
	getuboot(ontg_sn, ontg_pwd, wifi_mac,dev_mode,firmware_ver);

	//if(0!=strcmp(ontg_sn, sn))||
	if((0!=strcmp(ontg_pwd, pwd))||(0!=strcmp(wifi_mac, mac)))
	{
	if(hex_on==1)
	{
          const char *sss;
	  char *temp = (char *) malloc(20);
        char *temp1 = (char *) malloc(20);
        char *ontg_pwd_set = (char *) malloc(21);
		memset(ontg_pwd_set,0,sizeof(ontg_pwd_set));
	   for(sss = pwd; *sss != '\0'; sss++)
	   {
	 	      sprintf(temp,"%X",*sss);
	      strcat(ontg_pwd_set,temp);
	   }
	   if(strlen(ontg_pwd_set)!=20)
	   {
	        for ( i=0;i<(20-strlen(ontg_pwd_set));i++)
			{
				strcat(temp1,"0");
			}
		strcat(ontg_pwd_set,temp1);
	   }
	   if( !strcmp(ontg_pwd_set,"00000000000000000000" ) == 0 )
	     {
		    sprintf(setpwd,"fw_setenv ONTG_PWD %s\n",ontg_pwd_set);
		    system(setpwd);	
		    sleep(2);
	    }
	   free(temp);
	   free(temp1);
	   free(ontg_pwd_set);
	}
	else
	{
	   sprintf(setpwd,"fw_setenv ONTG_PWD %s\n",pwd);
	   system(setpwd);
	}
	   free(setpwd);
		//sprintf(cmd, "fw_setenv ONTG_SN %s\n", sn);
		//system(cmd);	
		//sprintf(cmd, "fw_setenv ONTG_PWD %s\n", pwd);
		//system(cmd);	
		sprintf(cmd, "fw_setenv macaddr %s\n", mac);
		system(cmd);
		
	}
	if((0!=strcmp(dev_mode, mode)))
	{
		sprintf(cmd, "fw_setenv opermode %s\n", mode);
		system(cmd);
		if(!strcmp(mode,"bridge"))
		{
			sprintf(cmd, "fw_setenv macbridge %s\n", "1");
			system(cmd);
		}
		else
		{
			sprintf(cmd, "fw_setenv macbridge %s\n", "0");
			system(cmd);
		}

		/* delete tr069 config file for change mode */
		brg_system(&status, NULL,
	       	"rm -rf %s/* ", 
		"/var/lib/tr069/data/");

	    brg_system(&status, NULL,
	       	"rm -rf %s/* ", 
		"/var/lib/tr069/parameter/");

		if (strcmp(mode,"bridge") == 0)
        {
#ifdef __LILAC__
            SET_PORT_OPERMODE( status, "eth0", "bridge" );
#endif            
            SET_PORT_OPERMODE( status, "eth1", "bridge" );
            SET_PORT_OPERMODE( status, "eth2", "bridge" );
            SET_PORT_OPERMODE( status, "eth3", "bridge" );
#ifndef __LILAC__
            SET_PORT_OPERMODE( status, "eth4", "bridge" );
#endif
            SET_PORT_OPERMODE( status, "wifi", "bridge" );
        }
        else
        {
#ifdef __LILAC__
            SET_PORT_OPERMODE( status, "eth0", "router" );
#endif            
            SET_PORT_OPERMODE( status, "eth1", "router" );
            SET_PORT_OPERMODE( status, "eth2", "router" );
            SET_PORT_OPERMODE( status, "eth3", "router" );
#ifndef __LILAC__
            SET_PORT_OPERMODE( status, "eth4", "router" );
#endif
            SET_PORT_OPERMODE( status, "wifi", "router" );
        }
		
		//system("/bl/bin/restore_factory_settings_mode.sh &");
		//system("reboot");
	}

    system("sync");

	return;	
}

char * ox_to_int(char *ONTG_PWD)
{
   char *p;
   char *p_tmp= (char *) malloc(3);
   char *temp = (char *) malloc(11);
   char* _temp;
   _temp=temp;
   int n;
  // int status;
   for(p=ONTG_PWD,temp;*p !='\0';p=p+2,temp++)
   	{
          memcpy(p_tmp,p,2);
	   n=(atoi(p_tmp)/10)*16+atoi(p_tmp+1);
	   (*temp)=(char)n;
	  // brg_system(&status, NULL,"echo 'The char of ONTG_PWD:%s' >> /tmp/sys_tmp",temp);
	  // brg_printf_console("****The char of ONTG_PWD:%d************\n",(int)p_tmp);
   	}
   return _temp;
}
static int sys_restart(void)
{
	static int init=0;
	int result = 0;
	FILE *fp;
	xmlNodePtr db,sys;
    //int status, enabled = brg_xml_get_path_int(db, "/ftp/enabled");
	brg_xml_val_t v_ont_status,v_ont_qos,v_ontg_sn,v_ontg_pwd, v_wifi_mac,v_dev_mode,v_firmware_ver;
	int v_hex_on=0,hex_on;
	char *ont_status,*ont_qos,*ontg_sn, *ontg_pwd, *wifi_mac, *dev_mode, *firmware_ver;
	char * hex_tmp;
	char ontg_sn_u[64], ontg_pwd_u[64], wifi_mac_u[64],dev_mode_u[64]={0},firmware_ver_u[64];
	//	brg_xml_val_t v_wifi_mactest;
	//char *wifi_mactest;
	char mac[64];
	char cmd[100] = {0};

    db = get_mgmt_db_root_by_state();
	ont_status = brg_xml_get_path_str(v_ont_status, db, "/system/ont_status");
	ont_qos = brg_xml_get_path_str(v_ont_qos, db, "/system/ont_qos");	
	ontg_sn = brg_xml_get_path_str(v_ontg_sn, db, "/system/ontg_sn");
	hex_on= brg_xml_get_path_int_base(db, "/system/hex_on",v_hex_on);
	ontg_pwd = brg_xml_get_path_str(v_ontg_pwd, db, "/system/ontg_pwd");
	wifi_mac = brg_xml_get_path_str(v_wifi_mac, db, "/system/wifi_mac");	
	dev_mode = brg_xml_get_path_str(v_dev_mode, db, "/system/dev_mode");
	firmware_ver=brg_xml_get_path_str(v_firmware_ver,db,"/system/firmware_ver");
	
	if(!strcmp(ont_status,"active"))
	{
		result =rgwapi_conctrol_pon_link(1);
		if (result != 0)
		{
			brg_printf_console("rgwapi_conctrol_pon_link(1) error\n");
		}
	}
	else if(!strcmp(ont_status,"inactive"))
	{
		result =rgwapi_conctrol_pon_link(0);
		if (result != 0)
		{
			brg_printf_console("rgwapi_conctrol_pon_link(0) error\n");
		}
	}
	else
	{
		brg_printf_console("pon link param error\n");
	}
	system("/sbin/fw_printenv macaddr | cut -d= -f2 > /tmp/sys");
      if ( ( fp = fopen ( "/tmp/sys", "r" ) ) == NULL )
	{
		brg_printf_console("open /tmp/sys failed!\n");
		//goto exit;
	}
	if(NULL==fgets(mac, 63, fp))
	{
		sprintf(cmd, "fw_setenv macaddr %s\n", wifi_mac);
		system(cmd);
	}
	fclose(fp);

	if(0==init)
	{
		getuboot(ontg_sn_u, ontg_pwd_u, wifi_mac_u,dev_mode_u,firmware_ver_u);
		if(strcmp(dev_mode_u,"router")&&strcmp(dev_mode_u,"hybrid")&&strcmp(dev_mode_u,"bridge"))
		{
			strcpy(dev_mode_u,"router");
		}
		if(hex_on == 1)
			hex_tmp=ox_to_int(ontg_pwd_u);
		else
			hex_tmp=ontg_pwd_u;
		if((0!=strcmp(ontg_sn_u, ontg_sn))||
		(0!=strcmp(hex_tmp, ontg_pwd))||
		(0!=strcmp(wifi_mac_u, wifi_mac))||(0!=strcmp(dev_mode_u, dev_mode))||(0!=strcmp(firmware_ver_u, firmware_ver)))
		{
			//brg_printf_console("write to db mac[%s]\n",wifi_mac_u);
			sys= brg_xml_get_path_single_verify(db, "%s", "/system");
			brg_xml_set_path_str(sys, "/ontg_sn", ontg_sn_u);
			brg_xml_set_path_str(sys, "/ontg_pwd", hex_tmp);
			brg_xml_set_path_str(sys, "/wifi_mac", wifi_mac_u);
			brg_xml_set_path_str(sys, "/dev_mode", dev_mode_u);
			brg_xml_set_path_str(sys, "/firmware_ver", firmware_ver_u);
			//wifi_mactest = brg_xml_get_path_str(v_wifi_mactest, db, "/system/wifi_mac");
			
			//brg_printf_console("get from db mac[%s]\n",wifi_mactest);
			server_commit();
		}
	
		//sync2db(ontg_sn, ontg_pwd, wifi_mac);

		
		init=1;
		return 0;
	}
	else
	{
		sync2uboot(ontg_sn,hex_on, ontg_pwd, wifi_mac,dev_mode);
	}
	
    return 0;
}

static mo_param_t sys_mo =
{
    name: "system",
    desc: "system configuration",
    log_module: BRG_LOG_MODULE_PLUGIN_SYSTEM,
    precommit:
    (precommit_cb_param_t [])
    {
	{
	    sys_restart, "/root/system"
	},
	{}
    },
};

DECLARE_MO(sys_mo);

