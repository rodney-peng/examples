/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

/**
 *
 * @author tw5015
 */
import gnu.io.CommPort;
import gnu.io.CommPortIdentifier;
import gnu.io.SerialPort;
import gnu.io.SerialPortEvent;
import gnu.io.SerialPortEventListener;

import java.io.*;

interface ConsoleDataInterface {
    public void ReceiveData( byte [] data, int length );
}

public class ODS_ProxyConsole implements ConsoleWindowInterface, ConsoleDataInterface {

    private ODS_ProxyView  appView;

    private CommPort      commPort;
    private InputStream   in_s;
    private OutputStream  out_s;

    public boolean isConnected = false;

    private ConsoleServer  server;

    public ODS_ProxyConsole ( ODS_ProxyView view ) {
        appView = view;

        appView.setConsoleHandler( this );
    }

    public void ConnectButtonClicked( String port, String baudRate, String serverPort ) {
        if (isConnected)
        {
            isConnected = false;

            server.Close();
            commPort.close();

            appView.WriteToConsoleWindow( "\nConsole closed!" );
            appView.WriteToConsoleWindow( "\nServer closed!" );
        }
        else if (port.length() > 0 && baudRate.length() > 0 && serverPort.length() > 0)
        {
            try {
                int  rate = Integer.parseInt( baudRate );
                int  srvport = Integer.parseInt( serverPort );

                if (rate > 0 && srvport > 0)
                {
                    if (connect( port, rate )) {
                        appView.WriteToConsoleWindow( "\nConsole connected!" );

                        server = new ConsoleServer( this, srvport );
                        if (server.isConnected) {
                            isConnected = true;

                            server.start();
                            appView.WriteToConsoleWindow( "\nServer started on port " + srvport + "!" );
                        }
                        else {
                            commPort.close();

                            appView.WriteToConsoleWindow( "\nCannot start server!" );
                        }
                    }
                    else {
                        appView.WriteToConsoleWindow( "\nCannot connect console!" );
                    }
                }
            }
            catch ( Exception e )
            {
                e.printStackTrace();
            }
        }

        if (isConnected)
        {
            appView.SetConsoleConnectButtonText( "Close" );
        }
        else
        {
            appView.SetConsoleConnectButtonText( "Connect" );
        }
}

    public void KeyTyped( char key ) {
        try {
            if (isConnected) {
                out_s.write( key );
                out_s.flush();
            }
        }
        catch ( IOException e )
        {
            e.printStackTrace();
        }
    }

    private boolean connect ( String portName, int baudRate ) throws Exception
    {
        boolean  connected = false;

        CommPortIdentifier portIdentifier = CommPortIdentifier.getPortIdentifier(portName);

        if ( portIdentifier.isCurrentlyOwned() )
        {
            System.out.println("Error: Port is currently in use");
        }
        else
        {
            commPort = portIdentifier.open(this.getClass().getName(),2000);

            if ( commPort instanceof SerialPort )
            {
                SerialPort serialPort = (SerialPort) commPort;

                serialPort.setSerialPortParams(baudRate,SerialPort.DATABITS_8,SerialPort.STOPBITS_1,SerialPort.PARITY_NONE);

                in_s = serialPort.getInputStream();
                out_s = serialPort.getOutputStream();

                serialPort.addEventListener(new SerialReader(in_s, this));
                serialPort.notifyOnDataAvailable(true);

                connected = true;
            }
            else
            {
                System.out.println("Error: Only serial ports are handled by this example.");
            }
        }

        return connected;
    }

    public void ReceiveData( byte [] data, int length ) {
        // Receive data from console port

        appView.WriteToConsoleWindow( new String( data, 0, length ) );

        try { server.WriteData(data, length); } catch ( IOException e ) { e.printStackTrace(); }
    }

    public void WriteData( byte [] data, int length ) throws IOException  {
        // Write data to console port

        if (isConnected) {
            out_s.write( data, 0, length );
            out_s.flush();
        }

        // For monitor purpose only
        //int  i;
        //for (i = 0; i < length; i++)
        //    appView.WriteToShellWindow( "("+ Byte.toString( data[i] ) +")" );
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

class SerialReader implements SerialPortEventListener
{
    private InputStream in_s;

    private ConsoleDataInterface  dataHandler;

    private byte[] buffer = new byte[1024];

    public SerialReader ( InputStream in, ConsoleDataInterface handler )
    {
        in_s = in;

        dataHandler = handler;
    }

    public void serialEvent(SerialPortEvent event) {
        int data;

        try
        {
            int len = 0;
            while ( ( data = in_s.read()) > -1 )
            {
                if (data != '\b')
                {
                    buffer[len++] = (byte) data;
                }
                else
                {
                    if (len > 0) dataHandler.ReceiveData( buffer, len);

                    buffer[0] = (byte)'\b';
                    dataHandler.ReceiveData( buffer, 1 );

                    len = 0;
                }

                if ( data == '\n' || len >= buffer.length)
                {
                    break;
                }
            }

            if (len > 0) dataHandler.ReceiveData( buffer, len);
        }
        catch ( IOException e )
        {
            e.printStackTrace();
        }
    }
}
