/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

/**
 *
 * @author tw5015
 */
public interface ConsoleWindowInterface {

    public void ConnectButtonClicked( String port, String baudRate, String serverPort );

    public void KeyTyped( char key );
}
