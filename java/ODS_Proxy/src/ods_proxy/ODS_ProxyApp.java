/*
 * ODS_ProxyApp.java
 */

package ods_proxy;

import org.jdesktop.application.Application;
import org.jdesktop.application.SingleFrameApplication;

import javax.swing.*;

/**
 * The main class of the application.
 */
public class ODS_ProxyApp extends SingleFrameApplication {

    static String [] appArgs;
    private ODS_ProxyConsole  console;
    private ODS_ProxyShell    shell;

    /**
     * At startup create and show the main frame of the application.
     */
    @Override protected void startup() {
        ODS_ProxyView  appView = new ODS_ProxyView(this);

        console = new ODS_ProxyConsole( appView );
        shell   = new ODS_ProxyShell( appView );

        show( appView );
    }

    /**
     * This method is to initialize the specified window by injecting resources.
     * Windows shown in our application come fully initialized from the GUI
     * builder, so this additional configuration is not needed.
     */
    @Override protected void configureWindow(java.awt.Window root) {
        this.getMainFrame().setTitle( "ODS Proxy" );
        this.getMainFrame().setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    }

    /**
     * A convenient static getter for the application instance.
     * @return the instance of ODS_ProxyApp
     */
    public static ODS_ProxyApp getApplication() {
        return Application.getInstance(ODS_ProxyApp.class);
    }

    /**
     * Main method launching the application.
     */
    public static void main(String[] args) {

        appArgs = args;

        launch(ODS_ProxyApp.class, args);
    }

    public String GetClients() {
        return console.GetClients() + "\r\n" + shell.GetClients();
    }
}
