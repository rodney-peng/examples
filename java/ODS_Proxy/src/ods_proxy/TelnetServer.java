/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

import java.net.*;
import java.io.*;

/**
 * TelnetServer is a server socket connected to a telnet client
 * @author tw5015
 */

public class TelnetServer extends Thread {

    static final int  CMD_UNLOCK = '0';
    static final int  CMD_LOCK = '1';
    static final int  CMD_SHOW_CLIENTS = '2';

    TelnetServerListener handler;

    Socket socket;
    InputStream   ins;
    OutputStream  outs;

    static final String  proxyCommandPrefix = "***ods ";
    int  commandMatchedLength = 0;
    int  proxyCommand = 0;

    static final String exitCommand = "exit";
    int  exitMatchedLength = 0;

    int currentInputLength = 0;

    public boolean locked = false;

    public TelnetServer( Socket s, TelnetServerListener listener, boolean locked ) throws IOException {
        handler = listener;

        socket = s;
        ins = socket.getInputStream();
        outs = socket.getOutputStream();

        this.locked = locked;

        if (locked) { EnterLockedMode(); }
    }

    public void run() {

        if ((! socket.isConnected()) || ins == null) return;

        int  state = TelnetConstant.STATE_NOP;
        int  command = 0;
        int  option  = 0;

        byte []  buffer = new byte[3];

        try {
            outs.write( TelnetConstant.IAC );
            outs.write( TelnetConstant.WILL );
            outs.write( TelnetConstant.OPT_ECHO );

            while (true) {
                int  ch = ins.read();
                if (ch < 0) break;

                //----------------------------------------------
                // The client sends two characters for <return> key
                // Other:     \r(13) and \n(10)
                // Tera Term: \r(13) and \0(0)

                switch (state)
                {
                case TelnetConstant.STATE_NOP:
                     if (ch == TelnetConstant.IAC)
                     {
                         state = TelnetConstant.STATE_IAC;
                     }
                     else if (! locked)
                     {
                         if (checkExitCommand( ch ))
                             ; // all done
                         else if(checkProxyCommand(ch))
                             ; // all done
                         else
                         {
                             if (commandMatchedLength > 0) {
                                 handler.ReceiveData( this, proxyCommandPrefix.getBytes(), commandMatchedLength );
                             }
                             
                             if (proxyCommand > 0) {
                                 buffer[0] = (byte)proxyCommand;
                                 handler.ReceiveData( this, buffer, 1 );
                             }

                             commandMatchedLength = 0;
                             proxyCommand = 0;
                             
                             if (exitMatchedLength > 0) {
                                 handler.ReceiveData( this, exitCommand.getBytes(), exitMatchedLength );
                             }

                             exitMatchedLength = 0;

                             buffer[0] = (byte)ch;
                             handler.ReceiveData( this, buffer, 1 );

                             switch (ch)
                             {
                             case '\b':
                                 if (currentInputLength > 0) currentInputLength--;
                                 break;
                             
                             case '\r':
                                 currentInputLength = 0;
                                 break;

                             default:
                                 if (31 < ch && ch < 127) //visible
                                     currentInputLength++;
                             }
                         }
                     }
                     else
                     {
                         // input is ignored and echoed with '*'
                         outs.write( '*' );
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
                         handler.ReceiveData( this, buffer, 2 );

                         state = TelnetConstant.STATE_NOP;
                     }
                     break;

                case TelnetConstant.STATE_CMD:
                    option = ch;

                    switch (command)
                    {
                    case TelnetConstant.WILL:
                    case TelnetConstant.WONT:
                        command = TelnetConstant.DONT;

                        outs.write( TelnetConstant.IAC );
                        outs.write( command );
                        outs.write( option );
                        break;

                    case TelnetConstant.DO:
                        if (option == TelnetConstant.OPT_ECHO)
                        {
                            outs.write( TelnetConstant.IAC );
                            outs.write( TelnetConstant.WILL );
                            outs.write( option );

                            break;
                        }
                        // falls through the next case
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
                        handler.ReceiveData( this, buffer, 3 );
                    }

                    state = TelnetConstant.STATE_NOP;
                    break;
                }
            }
        } catch (IOException e) { e.printStackTrace(); }

        try {
            socket.close();
            handler.OnSocketClosed( this );
        } catch (IOException e) { e.printStackTrace(); }
    }

    private boolean checkExitCommand( int ch ) throws IOException {
        if (currentInputLength > 0 || commandMatchedLength > 0) return false;

        boolean  handled = true;

        if (exitMatchedLength == exitCommand.length())
        {
            switch (ch)
            {
            //case '\n':
            case '\r':
                ins.read(); // read and discard '\n'

                outs.write( '\r' );
                outs.write( '\n' );

                exitMatchedLength = 0;

                handler.OnExit(this);
                break;

            case '\b':
                exitMatchedLength--;

                outs.write( '\b' );
                outs.write( ' ' );
                outs.write( '\b' );
                break;

            default:
                handled = false;
            }
        }
        else if (ch == exitCommand.charAt( exitMatchedLength ))
        {
            exitMatchedLength++;

            outs.write( ch ); // echo the character
        }
        else if (ch == '\b' && exitMatchedLength > 0)
        {
            exitMatchedLength--;

            outs.write( '\b' );
            outs.write( ' ' );
            outs.write( '\b' );
        }
        else
        {
            handled = false;
        }

        return handled;
    }

    private boolean checkProxyCommand( int ch ) throws IOException {
        if (currentInputLength > 0 || exitMatchedLength > 0) return false;

        boolean  handled = true;

        if (commandMatchedLength == proxyCommandPrefix.length())
        {
            switch (ch)
            {
            //case '\n' :
            case '\r':
                ins.read(); // read and discard '\n'

                outs.write( '\r' ); // 13
                outs.write( '\n' ); // 10

                commandMatchedLength = 0;

                if (proxyCommand > 0)
                {
                    executeCommand( proxyCommand );

                    proxyCommand = 0;
                }
                break;

            case '\b':
                commandMatchedLength--;

                outs.write( '\b' );
                outs.write( ' ' );
                outs.write( '\b' );
                break;

            default:
                outs.write( ch );

                if (proxyCommand == 0)
                {
                    if (31 < ch && ch < 127) // visible
                    {
                        proxyCommand = ch;
                    }
                    // else reset command
                }
                else
                {
                    proxyCommand = 0;
                }

                if (proxyCommand == 0)
                {
                    outs.write( '\r' );
                    outs.write( '\n' );

                    commandMatchedLength = 0;
                }
            }
        }
        else if (ch == proxyCommandPrefix.charAt( commandMatchedLength ))
        {
            commandMatchedLength++;

            outs.write( ch ); // echo the character
        }
        else if (ch == '\b' && commandMatchedLength > 0)
        {
            commandMatchedLength--;

            outs.write( '\b' );
            outs.write( ' ' );
            outs.write( '\b' );
        }
        else
        {
            handled = false;
        }

        return handled;
    }

    private void executeCommand( int command ) {
        try {

        switch (command)
        {
        case CMD_UNLOCK:
            handler.OnLockTerminalOff(this);
            final String unlockMessage = "OK unlocked\r\n";
            WriteData( unlockMessage.getBytes(), unlockMessage.length() );
            break;

        case CMD_LOCK:
            handler.OnLockTerminalOn(this);
            final String lockMessage = "OK locked\r\n";
            WriteData( lockMessage.getBytes(), lockMessage.length() );
            break;

        case CMD_SHOW_CLIENTS:
            handler.OnShowClients(this);
            break;

        default:
            // ignore the command
        }

        }
        catch ( IOException e ) { e.printStackTrace(); }
    }

    public synchronized void WriteData( byte [] data, int length ) throws IOException {
        if (socket.isConnected()) outs.write( data, 0, length );
    }

    public void Close() {
        try { socket.close(); } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void EnterLockedMode() {

        locked = true;

        commandMatchedLength = 0;
        proxyCommand = 0;

        exitMatchedLength = 0;

        final String lockMessage = "\r\nTerminal is locked!\r\n";
        try { WriteData( lockMessage.getBytes(), lockMessage.length() ); }
        catch ( IOException e ) { e.printStackTrace(); }
    }

    public void LeaveLockedMode() {
        locked = false;

        commandMatchedLength = 0;
        proxyCommand = 0;

        exitMatchedLength = 0;

        final String unlockMessage = "\r\nTerminal is unlocked!\r\n";
        try { WriteData( unlockMessage.getBytes(), unlockMessage.length() ); }
        catch ( IOException e ) { e.printStackTrace(); }
    }

    public String GetClient() {
        return socket.toString();
    }
}