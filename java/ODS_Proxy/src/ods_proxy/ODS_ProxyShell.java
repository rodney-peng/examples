/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

/**
 *
 * @author tw5015
 */

import java.net.*;
import java.io.*;

interface ShellDataInterface {
    public void ReceiveData( byte [] data, int length );
    public void OnSocketClosed();
}

public class ODS_ProxyShell implements ShellWindowInterface, ShellDataInterface {

    private ODS_ProxyView  appView;

    private Socket        odsSocket = null;
    private OutputStream  odsOutStream = null;

    private boolean isConnected = false;

    private ShellServer  server;

    public ODS_ProxyShell( ODS_ProxyView view ) {
        appView = view;

        appView.setShellHandler( this );
    }

    public void ConnectButtonClicked( String ipaddr, String telnetport, String serverPort ) {
        if (isConnected)
        {
            isConnected = false;

            if (server != null) server.Close();
            try { odsSocket.close(); } catch (IOException e) { e.printStackTrace(); }

            appView.WriteToShellWindow( "\nShell closed!" );

            //    Client session will be closd and the message will be printed when OnSocketClosed() is called.
            //    Nothing should be done here !
            //appView.WriteToShellWindow( "Server closed !" );
        }
        else if (ipaddr.length() > 0 && telnetport.length() > 0 && serverPort.length() > 0)
        {
            try {
                int  port = Integer.parseInt( telnetport );
                int  srvport = Integer.parseInt( serverPort );

                if (port > 0 && srvport > 0)
                {
                    odsSocket    = new Socket( ipaddr, port );
                    odsOutStream = odsSocket.getOutputStream();

                    server = new ShellServer( this, srvport );

                    (new ReadTelnetServer(odsSocket, this)).start();
                    server.start();

                    if (odsSocket.isConnected())
                    {
                        appView.WriteToShellWindow( "\nShell connected!" );

                        if (server.isConnected) {
                            isConnected = true;

                            appView.WriteToShellWindow( "\nServer started on port " + serverPort + "!" );
                        }
                        else {
                            odsSocket.close();

                            appView.WriteToShellWindow( "\nCannot start server!" );
                        }
                    }
                    else
                    {
                        appView.WriteToShellWindow( "\nCannot connect shell!" );
                    }
                }
            }
            catch ( Exception e )
            {
                e.printStackTrace();

                if (! isConnected) appView.WriteToShellWindow( "\nCannot connect shell!" );
            }
        }

        if (isConnected)
        {
            appView.SetShellConnectButtonText( "Close" );
        }
        else
        {
            appView.SetShellConnectButtonText( "Connect" );
        }
    }

    public void KeyTyped( char key ) {
        try {
            if (isConnected) {
                if (key == '\n') // sends both '\r' and '\n' for <return key>
                    odsOutStream.write( (int)(byte)'\r' );

                odsOutStream.write( (int)(byte)key );
            }
        } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void ReceiveData( byte [] data, int length ) {
        // Receive data from telnet server

        appView.WriteToShellWindow( new String( data, 0, length ) );

        try { server.WriteData(data, length); } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void OnSocketClosed() {
        if (server != null) {
            // This happened because the socket is closed before ShellServer is created
            server.Close();
        }

        isConnected = false;
        appView.SetShellConnectButtonText( "Connect" );

        appView.WriteToShellWindow( "\nServer closed!" );
    }

    public void WriteData( byte [] data, int length ) throws IOException {
        // Write data to telnet server

        if (isConnected) {
            odsOutStream.write( data, 0, length );
            odsOutStream.flush();
        }

        // For monitor purpose only
        //int  i;
        //for (i = 0; i < length; i++)
        //    appView.WriteToConsoleWindow( "("+ Byte.toString( data[i] ) +")" );
    }

    public String GetClients() {
        if (isConnected && (server != null))
        {
            return server.GetClients();
        }
        else
        {
            return "";
        }
    }
}

class ReadTelnetServer extends Thread {
    Socket socket;

    InputStream  ins;
    OutputStream  outs;

    ShellDataInterface  dataHandler;

    public ReadTelnetServer(Socket s, ShellDataInterface handler) throws Exception {
        socket = s;

        ins  = socket.getInputStream();
        outs = socket.getOutputStream();

        dataHandler = handler;
    }

    public void run() {

        if ((! socket.isConnected()) || ins == null) return;

        int  state = TelnetConstant.STATE_NOP;
        int  command = 0;
        int  option  = 0;

        byte []  buffer = new byte[3];

        try {
            while (true) {
                int  ch = ins.read();
                if (ch < 0) break;

                switch (state)
                {
                case TelnetConstant.STATE_NOP:
                     if (ch == TelnetConstant.IAC)
                     {
                         state = TelnetConstant.STATE_IAC;
                     }
                     else
                     {
                         buffer[0] = (byte)ch;
                         dataHandler.ReceiveData( buffer, 1 );
                     }
                     break;

                case TelnetConstant.STATE_IAC:
                     switch (ch)
                     {
                     case TelnetConstant.WILL:
                     case TelnetConstant.WONT:
                     case TelnetConstant.DO:
                     case TelnetConstant.DONT:
                         command = ch;
                         state = TelnetConstant.STATE_CMD;
                         break;

                     default:
                         buffer[0] = (byte)TelnetConstant.IAC;
                         buffer[1] = (byte)ch;
                         dataHandler.ReceiveData( buffer, 2 );

                         state = TelnetConstant.STATE_NOP;
                     }
                     break;

                case TelnetConstant.STATE_CMD:
                    option = ch;

                    switch (command)
                    {
                    case TelnetConstant.WILL:
                        if (option == TelnetConstant.OPT_ECHO)
                        {
                            // remains silent
                            break;
                        }
                        // falls through the next case
                    case TelnetConstant.WONT:
                        command = TelnetConstant.DONT;

                        outs.write( TelnetConstant.IAC );
                        outs.write( command );
                        outs.write( option );
                        break;

                    case TelnetConstant.DO:
                    case TelnetConstant.DONT:
                        command = TelnetConstant.WONT;

                        outs.write( TelnetConstant.IAC );
                        outs.write( command );
                        outs.write( option );
                        break;

                    default:
                        buffer[0] = (byte)TelnetConstant.IAC;
                        buffer[1] = (byte)command;
                        buffer[2] = (byte)option;
                        dataHandler.ReceiveData( buffer, 3 );
                    }

                    state = TelnetConstant.STATE_NOP;
                    break;
                }
            }
        } catch (Exception e) { e.printStackTrace(); }

        try {
            socket.close();
            dataHandler.OnSocketClosed();
        }  catch (Exception e) { e.printStackTrace(); }
    }
}
