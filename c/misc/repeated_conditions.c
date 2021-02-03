/*
    Use macro to handle repeated conditions
    A loop to verify no conflict
*/

OpS32 ProvCheckVoiceProfileApi(tVoiceProfile *pProfile, const char ** ppInfo)
{
#define CODEC_SELECTIONS  6

#define DO_CODECS \
    DO_CODEC( u321stCodec, && ) \
    DO_CODEC( u322ndCodec, && ) \
    DO_CODEC( u323rdCodec, && ) \
    DO_CODEC( u324thCodec, && ) \
    DO_CODEC( u325thCodec, && ) \
    DO_CODEC( u326thCodec, )

    OpS32 ret = 0;
    sVoiceProfCodec codecs[CODEC_SELECTIONS];
    OpBool bConflict = OpFalse;
    int  i, j;

    if (pProfile->bDigitmapEnable && (pProfile->sDigitmap[0] == '\0'))
    {
        if (ppInfo) *ppInfo = "Digitmap is enabled but empty!";
        return -1;
    }

    #define DO_CODEC(codec, op) (pProfile->codec == 0) op
    if (DO_CODECS)
    {
        if (ppInfo) *ppInfo = "At least one codec should be enabled!";
        return -1;
    }
    #undef DO_CODEC

    // rate = 0 if disabled
    // rate = 20 if G.722
    #define DO_CODEC(codec, op) \
    if (pProfile->codec == 0) \
        pProfile->codec##PacketizationRate = 0; \
    if (pProfile->codec == 10) \
        pProfile->codec##PacketizationRate = 20;

    DO_CODECS
    #undef DO_CODEC

    memcpy( &codecs, &pProfile->u321stCodec, sizeof codecs );

    // verify no conflict
    for (i = 0; (i < CODEC_SELECTIONS) && (! bConflict); i++)
        for (j = i + 1; (codecs[i].Codec != 0) && (j < CODEC_SELECTIONS) && (! bConflict); j++)
            bConflict = (codecs[i].Codec == codecs[j].Codec);

    if (bConflict)
    {
        if (ppInfo) *ppInfo = "Codec selections conflict!";
        return -1;
    }

	return ret;

#undef DO_CODECS
}

