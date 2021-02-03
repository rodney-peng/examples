int Decompose_SIP_URI( const char * sipURI, char * protocol, char * user, char * password, char * host, OpU16 * port );

#define IS_SIP_URI_VALID( uri )                Decompose_SIP_URI( uri, NULL, NULL, NULL, NULL, NULL )
#define SIP_URI_GET_PROTOCOL( uri, protocol )  Decompose_SIP_URI( uri, protocol, NULL, NULL, NULL, NULL )
#define SIP_URI_GET_USER( uri, user )          Decompose_SIP_URI( uri, NULL, user, NULL, NULL, NULL )
#define SIP_URI_GET_PASSWORD( uri, password )  Decompose_SIP_URI( uri, NULL, NULL, password, NULL, NULL )
#define SIP_URI_GET_HOST( uri, host )          Decompose_SIP_URI( uri, NULL, NULL, NULL, host, NULL )
#define SIP_URI_GET_PORT( uri, port )          Decompose_SIP_URI( uri, NULL, NULL, NULL, NULL, port )

// empty string is considered valid
static int isDecimalNumber( const char * str )
{
    while (*str && '0' <= *str && *str <= '9') str++;

    return *str == '\0';
}

// protocol:[user[:password]@]host[:port][;uri-parameters?headers]
int Decompose_SIP_URI( const char * sipURI, char * protocol, char * user, char * password, char * host, OpU16 * port )
{
    char buf[512];
    int len;
    char * at;
    const char * start = sipURI;
    const char * end = strchr( start, ';' );
    if (end == NULL) end = strchr( start, '\0' );
    len = end - start;

    if (len == 0 || len >= sizeof buf) return -1;
    memcpy( buf, start, len );
    buf[len] = '\0';
    start = buf;

    if (protocol) *protocol = '\0';
    if (user)     *user = '\0';
    if (password) *password = '\0';
    if (host)     *host = '\0';
    if (port)     *port = 0;

    if ((end = strchr( start, ':' )) == NULL) return -1;
    if (end == start) return -1;

    at = (char *)strchr( start, '@' );
    if (at != NULL && at < end) return -1;

    len = end - start;
    if (protocol)
    {
        memcpy( protocol, start, len );
        protocol[len] = '\0';
    }

    start += (len + 1);

    if (at != NULL)
    {
        const char * userinfo = start;
        int    infolen;

        if (at == start) return -1;

        infolen = at - start;
        *at = '\0';

        if ((end = strchr( start, ':' )) == NULL)
            end = strchr( start, '\0' );
        if (end == start) return -1;

        len = end - start;
        if (user)
        {
            memcpy( user, start, len );
            user[len] = '\0';
        }

        start += len;

        if (*start == ':')
        {
            len = infolen - len - 1;

            if (len && password)
            {
                memcpy( password, start + 1, len );
                password[len] = '\0';
            }
        }

        start = userinfo + infolen + 1;
    }

    if ((end = strchr( start, ':' )) == NULL)
        end = strchr( start, '\0' );
    if (end == start) return -1;

    len = end - start;
    if (host)
    {
        memcpy( host, start, len );
        host[len] = '\0';
    }

    start += len;

    if (*start == ':' && *(start+1))
    {
        int portnum;

        start++;

        if (! isDecimalNumber( start )) return -1;
        if ((portnum = atoi( start )) <= 0) return -1;
        if (portnum > 65535) return -1;

        if (port) *port = portnum;
    }

    return 0;
}

