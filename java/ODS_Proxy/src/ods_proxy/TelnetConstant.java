/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ods_proxy;

/**
 *
 * @author tw5015
 */
public class TelnetConstant {
  /** WILL see rfc854 */
  public final static int WILL = 251;
  /** WONT see rfc854 */
  public final static int WONT = 252;
  /** DO see rfc854 */
  public final static int DO   = 253;
  /** DON'T see rfc854 */
  public final static int DONT = 254;
  /** Interpret As Command */
  public final static int IAC  = 255;

  public final static int STATE_NOP = 0;
  public final static int STATE_IAC = 1;
  public final static int STATE_CMD = 2;

  public final static int OPT_ECHO = 1;
}
