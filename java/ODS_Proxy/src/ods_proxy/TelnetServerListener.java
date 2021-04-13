/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

/**
 *
 * @author tw5015
 */
public interface TelnetServerListener {

    // TelnetServer is a server socket connected to a telnet client

    public void ReceiveData( TelnetServer client, byte [] data, int length );
    public void OnSocketClosed( TelnetServer client );

    public void OnExit( TelnetServer client );
    public void OnLockTerminalOn( TelnetServer client );
    public void OnLockTerminalOff( TelnetServer client );
    public void OnShowClients( TelnetServer client );
}
