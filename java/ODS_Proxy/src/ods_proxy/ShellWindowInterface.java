/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

/**
 *
 * @author tw5015
 */
public interface ShellWindowInterface {

    public void ConnectButtonClicked( String ipaddr, String telnetport, String serverPort );

    public void KeyTyped( char key );
}
