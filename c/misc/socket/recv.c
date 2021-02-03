
    index = 0;
    do
    {
        len = recv(s, buf + index, BUF_SIZE - index, 0);
        if (len > 0) index += len;

    } while ((len > 0) || (len < 0 && (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK)));

