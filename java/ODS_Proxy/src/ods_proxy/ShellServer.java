 /*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

import java.net.*;
import java.io.*;

/**
 *
 * @author tw5015
 */
public class ShellServer extends Thread implements TelnetServerListener {

    ODS_ProxyShell shell;
    int listeningPort;
    public boolean isConnected = false;

    ServerSocket socket;

    TelnetServer []  clients = new TelnetServer[GlobalConfig.MAX_CLIENT_CONNECTIONS];
    TelnetServer  lockedClient = null;

    static final Object clientsLock = new Object();

    public ShellServer( ODS_ProxyShell shell, int port ) {
        this.shell = shell;
        listeningPort = port;

        try {
            socket = new ServerSocket( listeningPort );
            isConnected = true;
        } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void run() {
        if (! isConnected) return;
        try {
            int  i;
            boolean  socketAvailable = false;

            while (true)
            {
                socketAvailable = false;

                Socket client = socket.accept();

                synchronized (clientsLock) {
                for (i = 0; i < clients.length; i++)
                {
                    if (clients[i] == null)
                    {
                        clients[i] = new TelnetServer( client, this, lockedClient != null );
                        clients[i].start();

                        socketAvailable = true;
                        break;
                    }
                }
                }

                if (socketAvailable)
                {
                    System.out.println("Accept Connection From : " + client.getInetAddress() + ":" + Integer.toString(client.getPort()) );
                }
                else
                {
                    client.close();

                    System.out.println("No socket is available !");
                }
            }
        } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void ReceiveData( TelnetServer client, byte [] data, int length ) {
        try { shell.WriteData( data, length ); } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void OnSocketClosed( TelnetServer client ) {
        int  i;

        synchronized (clientsLock) {
        if (lockedClient != null && lockedClient == client) {
            for (i = 0; i < clients.length; i++)
            {
                if (clients[i] != null && clients[i] != client) { clients[i].LeaveLockedMode(); }
            }

            lockedClient = null;
        }

        for (i = 0; i < clients.length; i++)
        {
            if (clients[i] != null && clients[i] == client) {
                clients[i] = null;
            }
        }
        }
    }

    public void OnExit( TelnetServer client ) {

        synchronized (clientsLock) {
        if (lockedClient != null && lockedClient == client) {
            int  i;

            for (i = 0; i < clients.length; i++)
            {
                if (clients[i] != null && clients[i] != client) { clients[i].LeaveLockedMode(); }
            }

            lockedClient = null;
        }
        }

        client.Close();
    }

    public void OnLockTerminalOn( TelnetServer client ) {

        synchronized (clientsLock) {
        if (lockedClient == null) {
            lockedClient = client;

            int  i;

            for (i = 0; i < clients.length; i++)
            {
                if (clients[i] != null && clients[i] != client) { clients[i].EnterLockedMode(); }
            }
        }
        }
    }

    public void OnLockTerminalOff( TelnetServer client ) {

        synchronized (clientsLock) {
        if (lockedClient == client) {
            int  i;

            for (i = 0; i < clients.length; i++)
            {
                if (clients[i] != null && clients[i] != client) { clients[i].LeaveLockedMode(); }
            }
            
            lockedClient = null;
        }
        }
    }

    public void OnShowClients( TelnetServer client ) {
        String  clients = GetClients();
        
        try {
            client.WriteData( clients.getBytes(), clients.length() );
        } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void WriteData( byte [] data, int length ) throws IOException {
        int  i;

        synchronized (clientsLock) {
        for (i = 0; i < clients.length; i++)
        {
            if (clients[i] != null) { clients[i].WriteData(data, length); }
        }
        }
    }

    public void Close() {

        int  i;

        synchronized (clientsLock) {
        for (i = 0; i < clients.length; i++)
        {
            if (clients[i] != null) {

                clients[i].Close();
                clients[i] = null;
            }
        }
        }

        try {
            socket.close();
            isConnected = false;
            
        } catch ( IOException e ) { e.printStackTrace(); }
    }

    public String GetClients() {
        String  text = "Active shell client :\r\n";
        String  client = "";

        int  i;
        int  numActiveClient = 0;

        for (i = 0; i < clients.length; i++)
        {
            if (clients[i] != null) {
                numActiveClient++;

                client = "";

                if (clients[i].locked)
                {
                    client += "(locked)";
                }
                else
                {
                    client += "        ";
                }
                client += (Integer.toString( numActiveClient ) + " : " + clients[i].GetClient() + "\r\n");
                text += client;
            }
        }
        
        return text;
    }
}
