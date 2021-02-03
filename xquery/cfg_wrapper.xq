(: XQuery version of cfg_wrapper :)

(: write to file:
	Qexo:  write-to( $seq, $file )
	BaseX: file:write-text-lines( $file, $seq )
	XQilla ?
:)

declare variable $tab := "&#x9;";
declare variable $nl  := "&#xA;";

declare variable $charsToUnderscore := '.-/()"'' ';

declare function local:string-pad( $padString as xs:string?, $padCount as xs:integer) as xs:string 
{
   fn:string-join(for $i in 1 to $padCount return $padString)
};

declare function local:format_enum_name( $name )
{
	translate(upper-case($name), $charsToUnderscore, local:string-pad('_', string-length( $charsToUnderscore )))
};

declare function local:format_enum( $prefix, $name, $id )
{
	if (number($id) != 0) then
		concat( "    e", local:format_enum_name($prefix), local:format_enum_name($name), ' = ', $id, ',' )
	else
		concat( "    e", local:format_enum_name($prefix), local:format_enum_name($name) )
};

declare function local:format_enum_typename( $name )
{
	concat( "} e" , local:format_enum_name($name), ';' )
};

declare function local:format_struct_name( $name )
{
	translate($name, $charsToUnderscore, '')
};

declare function local:format_struct_decl( $type, $prefix, $name, $size )
{
	if ($prefix = "a" or $prefix = "s") then
		concat( $tab, $type, $tab, $name, '[', $size, '];' )
	else
		concat( $tab, $type, $tab, $name, ';' )
};

declare function local:itemSize( $item )
{
	switch ($item/@Type)
	case "U8"
	case "S8"
	case "YN"
	case "ABLE"
		return 1
	case "U16"
	case "S16"
		return 2
	case "IPV4"
	case "IPV4MASK"
	case "LIST"
	case "RELATION"
	case "U32"
	case "S32"
		return 4
	case "MAC"
		return 6
	case "FLOAT"
		return 20 (: as string :)
	case "STRING"
	case "FILESELECT"
	case "PASSWD" return
		if (empty( $item/@MaxValue )) then
			4
		else
			number( $item/@MaxValue )+1
	default return 0
};

declare function local:mcp_size( $items )
{
	(: accommodate ADMIN mcp, check local:mcpType() below :)

	4 + 4 + (6 * count( $items )) +
	sum(
		for $item in $items
		return local:itemSize( $item )
	)
};

declare function local:max_mcp_size( $descs )
{
	max(
		for $desc in $descs
		return local:mcp_size( $desc/* )
	)
};

declare function local:enumLevel1( $name, $prefix, $descs )
{
	let $prefix_ := concat($prefix, '_')
	return
	(
		"typedef enum {",
		for $desc in $descs
			return local:format_enum( $prefix_, $desc/@Name, $desc/@id ),
		'',
		local:format_enum( $prefix_, "FIRST_ID", $descs[1]/@id ),
		local:format_enum( $prefix_, "LAST_ID", $descs[last()]/@id ),
		local:format_enum( $prefix_, "MIN_ID", string(min($descs/@id)) ),
		local:format_enum( $prefix_, "MAX_ID", string(max($descs/@id)) ),
		local:format_enum( $prefix_, "ID_COUNT", count($descs/@id) ),
		local:format_enum( $prefix_, "END", 0 ),
		local:format_enum_typename( $prefix ),
		'',
		concat( "#define ", local:format_enum_name($name), "_MAX_MCP_SIZE", $tab, string(local:max_mcp_size( $descs )) ),
		''
	)
};

declare function local:enumLevel2( $descs )
{
	for $desc in $descs
	let $prefix := concat($desc/@Name, '_')
	let $items := $desc/*
	return
		if (count( $items ) != 0) then (
			"typedef enum {",
			for $item in $items
				return local:format_enum( $prefix, $item/@Name, $item/@id )
			,
			'',
			local:format_enum( $prefix, "FIRST_ID", $items[1]/@id ),
			local:format_enum( $prefix, "LAST_ID", $items[last()]/@id ),
			local:format_enum( $prefix, "MIN_ID", string(min($items/@id)) ),
			local:format_enum( $prefix, "MAX_ID", string(max($items/@id)) ),
			local:format_enum( $prefix, "ID_COUNT", count($items/@id) ),
			local:format_enum( $prefix, "MCP_SIZE", string(local:mcp_size($items)) ),
			local:format_enum( $prefix, "END", 0 ),
			local:format_enum_typename( $desc/@Name ),
			''
		)
		else ''
};

declare function local:itemType( $item )
{
	switch ($item/@Type)
	case "ABLE"
	case "YN"
	case "U8"
		return "OpU8"
	case "S8"
		return "OpS8"
	case "U16"
		return "OpU16"
	case "S16"
		return "OpS16"
	case "IPV4"
	case "IPV4MASK"
	case "LIST"
	case "RELATION"
	case "U32"
		return "OpU32"
	case "S32"
		return "OpS32"
	case "MAC"
		return "OpU8"
	case "FLOAT"
		return "FLOAT"
	case "STRING"
	case "FILESELECT"
	case "PASSWD" return
		if (empty( $item/@MaxValue )) then
			"OpS8 *"
		else
			"OpS8"
	default return ''
};

declare function local:itemPrefix( $item )
{
	switch ($item/@Type)
	case "U8"
		return "u8"
	case "S8"
		return "s8"
	case "YN"
	case "ABLE"
		return "b"
	case "U16"
		return "u16"
	case "S16"
		return "s16"
	case "IPV4"
	case "IPV4MASK"
	case "LIST"
	case "RELATION"
	case "U32"
		return "u32"
	case "S32"
		return "s32"
	case "MAC"
		return "a"
	case "FLOAT"
		return "f"
	case "STRING"
	case "FILESELECT"
	case "PASSWD" return
		if (empty( $item/@MaxValue )) then
			"p"
		else
			"s"
	default return ''
};

declare function local:itemStructName( $item )
{
	concat(local:itemPrefix( $item ), local:format_struct_name( $item/@Name ))
};

declare function local:structDecl( $item )
{
	local:format_struct_decl( local:itemType( $item ), local:itemPrefix( $item ), local:itemStructName( $item ), local:itemSize( $item ) )
};

declare function local:structTypeName( $name )
{
	concat('t', local:format_struct_name( $name ))
};

declare function local:descStruct( $desc )
{
	(
		"typedef struct {",
		for $item in $desc/*
			return local:structDecl( $item ),
		concat( "} " , local:structTypeName( $desc/@Name ), ';' )
	)
};

declare function local:funcDecl( $ret, $name, $args, $body )
{
	if (empty( $body )) then
		concat( $ret, " ", $name, "( ", $args, " );" )
	else
		(concat( $ret, " ", $name, "( ", $args, " )" ), "{", $body, "}")
};

declare function local:toMcpDataDecl( $name, $type, $prefix, $bHasCount, $body )
{
	if ($bHasCount) then
		local:funcDecl( "void", concat("Cfg", $prefix, "_", $name, "_toMcpDataApi"), concat("OpU16 count, ", $type, " * pData, OpU8 *pBuf, OpU16 *pOffset"), $body )
	else
		local:funcDecl( "void", concat("Cfg", $prefix, "_", $name, "_toMcpDataApi"), concat($type, " * pData, OpU8 *pBuf, OpU16 *pOffset"), $body )
};

declare function local:parseMcpDataDecl( $name, $type, $prefix, $body as xs:string* )
{
	local:funcDecl( "void", concat("Cfg", $prefix, "_", $name, "_parseMcpDataApi"), concat($type, " * pCfg, OpU16 id2, OpU8 *pData, OpU16 dataLen"), $body )
};

declare function local:DescMcpDecl( $name, $type, $prefix, $bHasCount )
{
	(
		local:toMcpDataDecl( $name, $type, $prefix, $bHasCount, () ),
		local:parseMcpDataDecl( $name, $type, $prefix, () )
	)
};

declare function local:DescGetDbDecl( $descType, $name )
{
	if ($descType = "TABLE") then
	(
		concat( "#define CfgEntryGet_", $name, "_DbByIndexApi(x, y)", $tab, "CfgEntryGet_", $name,  "_DataByIndexApi((x), (y), OpFalse)" ),
		concat( "#define CfgEntryGet_", $name, "_DbByKeyApi(x, y)", $tab, "CfgEntryGet_", $name,  "_DataByKeyApi((x), (y), OpFalse)" )
	)
	else if ($descType = "PORT") then
		concat( "#define CfgGet_", $name, "_DbApi(x, y)", $tab, "CfgGet_", $name,  "_DataApi((x), (y), OpFalse)" )
	else
		concat( "#define CfgGet_", $name, "_DbApi(x)", $tab, "CfgGet_", $name,  "_DataApi((x), OpFalse)" )
};

declare function local:DescGetDecl( $descType, $name, $dataType, $getBody, $trGetBody, $indexGetBody )
{
	if ($descType = "TABLE") then
	(
		local:funcDecl( "void", concat("CfgEntryGet_", $name, "_DataByKeyApi"), concat($dataType, " * pData, OpU16 key,  OpBool current"), $getBody ),
		local:funcDecl( "void", concat("CfgEntryGet_", $name, "_TR069_DataByKeyApi"), concat($dataType, " * pData, OpU16 key"), $trGetBody ),
		local:funcDecl( "void", concat("CfgEntryGet_", $name, "_DataByIndexApi"), concat($dataType, " * pData, OpU16 index,  OpBool current"), $indexGetBody )
	)
	else if ($descType = "PORT") then
	(
		local:funcDecl( "void", concat("CfgGet_", $name, "_DataApi"), concat($dataType, " * pData, OpU16 port,  OpBool current"), $getBody ),
		local:funcDecl( "void", concat("CfgGet_", $name, "_TR069_DataApi"), concat($dataType, " * pData, OpU16 port"), $trGetBody )
	)
	else
	(
		local:funcDecl( "void", concat("CfgGet_", $name, "_DataApi"), concat($dataType, " * pData, OpBool current"), $getBody ),
		local:funcDecl( "void", concat("CfgGet_", $name, "_TR069_DataApi"), concat($dataType, " * pData"), $trGetBody )
	)
};

declare function local:DescSaveDecl( $descType, $name, $dataType, $body )
{
	if ($descType = "TABLE") then
		local:funcDecl( "OpS32", concat("CfgEntry_", $name, "_SaveByKeyApi"), concat($dataType, " * pData, OpS32 key, OpBool bAdd, OpBool apply"), $body )
	else if ($descType = "PORT") then
		local:funcDecl( "OpS32", concat("Cfg_", $name, "_SaveApi"), concat($dataType, " * pData, OpU16 port, OpBool apply"), $body )
	else
		local:funcDecl( "OpS32", concat("Cfg_", $name, "_SaveApi"), concat($dataType, " * pData, OpBool apply"), $body )
};

declare function local:mcpPrefix( $desc )
{
	if ($desc/name() = "SettingDesc") then 'Entry' else ''
};

declare function local:mcpHasCount( $desc )
{
	($desc/name() = "DataListDesc")
};

declare function local:descType( $desc )
{
	if (exists( $desc/@Table )) then
		"TABLE"
	else if (exists( $desc/@PortNumber ) and (number($desc/@PortNumber) > 1)) then
		"PORT"
	else
		''
};

declare function local:tagNameEnum( $tag )
{
	concat("ePROVISION_DB_DESC_", $tag)
};

declare function local:tableDescDecl( $desc )
{
	let $name := local:format_struct_name( $desc/@Name )
	let $tagDescId := concat( local:tagNameEnum( $desc/../name() ), ", ", $desc/@id )
	return
	(
		concat( "#define CfgEntryGet_", $name, "_RecNumApi()",       $tab, "ProvisionDBTableGetTotalRecordNumberApi(", $tagDescId, ")" ),
		concat( "#define CfgEntry_", $name, "_AddApi(num)",          $tab, "ProvisionDBTableAddRecordApi(", $tagDescId, ", -1, (num))" ),
		concat( "#define CfgEntry_", $name, "_DelByKeyApi(key)",     $tab, "ProvisionDBTableDelRecordByKeyApi(", $tagDescId, ", (key), OpFalse)" ),
		concat( "#define CfgEntry_", $name, "_DelByIndexApi(index)", $tab, "ProvisionDBTableDelRecordByIndexApi(", $tagDescId, ", (index), OpFalse)" )
	)
};

declare function local:descDecl( $desc )
{
	let $name := local:format_struct_name( $desc/@Name )
	let $type := local:structTypeName( $desc/@Name )
	let $descType := local:descType($desc)
	return
	(
		local:DescMcpDecl( $name, $type, local:mcpPrefix($desc), local:mcpHasCount($desc) ),
		'',
		local:DescGetDbDecl( $descType, $name ),
		'',
		local:DescGetDecl( $descType, $name, $type, (), (), () ),
		'',
		local:DescSaveDecl( $descType, $name, $type, () ),
		if ($descType = 'TABLE') then ('', local:tableDescDecl( $desc ), '') else ''
	)
};

declare function local:itemTypeName( $item )
{
	upper-case( $item/@Type )
};

declare function local:itemFuncDecl( $name, $args, $toName, $toArgs, $item )
{
	let $define := concat( "#define ", $name, "(", $args, ")" )
	let $indent := if (string-length($define) > 80) then
						local:string-pad( $tab, 2 )
					else
						local:string-pad( $tab, xs:integer((80 - string-length($define)) div 4)+1 )
	let $toName := concat( $toName, local:itemTypeName($item) )
	let $toArgs := concat( local:tagNameEnum($item/../../name()), ", ", $item/../@id, ", ", $item/@id, if ($args = "") then "" else ", ", $toArgs )
	return concat( $define, $indent, $toName, "(", $toArgs, ")" )
};

declare function local:itemBoolDecl( $descType, $name, $item )
{
	if ($descType = "TABLE") then
	(
		local:itemFuncDecl( concat("CfgEntryIs_", $name, "_ByKeyApi"), "key, current", "CfgGetByKey", "current, key", $item ),
		local:itemFuncDecl( concat("CfgEntryIs_", $name, "_TR069_ByKeyApi"), "key", "CfgGetTmpByKey", "key", $item ),
		local:itemFuncDecl( concat("CfgEntryIs_", $name, "_ByIndexApi"), "index, current", "CfgGetByIndex", "current, index", $item ),
		local:itemFuncDecl( concat("CfgEntrySet_", $name, "_Api"), "key, data, apply, save", "CfgSetByKey", "data, apply, save, key", $item )
	)
(:
	else if ($descType = "PORT") then
	(
		local:itemFuncDecl( concat("CfgIs_", $name, "_Api"), "port, current", $item ),
		local:itemFuncDecl( concat("CfgIs_", $name, "_TR069_Api"), "port", $item ),
		local:itemFuncDecl( concat("CfgSet_", $name, "_Api"), "port, data, apply, save", $item )
	)
:)
	else
	(
		local:itemFuncDecl( concat("CfgIs_", $name, "_Api"), "current", "CfgGet", "current", $item ),
		local:itemFuncDecl( concat("CfgIs_", $name, "_TR069_Api"), "", "CfgGetTmp", "", $item ),
		local:itemFuncDecl( concat("CfgSet_", $name, "_Api"), "data, apply, save", "CfgSet", "data, apply, save", $item )
	)
};

declare function local:itemBasicDecl( $descType, $name, $item )
{
	if ($descType = "TABLE") then
	(
		local:itemFuncDecl( concat("CfgEntryGet_", $name, "_ByKeyApi"), "key, current", "CfgGetByKey", "current, key", $item ),
		local:itemFuncDecl( concat("CfgEntryGet_", $name, "_TR069_ByKeyApi"), "key", "CfgGetTmpByKey", "key", $item ),
		local:itemFuncDecl( concat("CfgEntryGet_", $name, "_ByIndexApi"), "index, current", "CfgGetByIndex", "current, index", $item ),
		local:itemFuncDecl( concat("CfgEntrySet_", $name, "_Api"), "key, data, apply, save", "CfgSetByKey", "data, apply, save, key", $item )
	)
(:
	else if ($descType = "PORT") then
	(
		local:itemFuncDecl( concat("CfgGet_", $name, "_Api"), "port, current", $item ),
		local:itemFuncDecl( concat("CfgGet_", $name, "_TR069_Api"), "port", $item ),
		local:itemFuncDecl( concat("CfgSet_", $name, "_Api"), "port, data, apply, save", $item )
	)
:)
	else
	(
		local:itemFuncDecl( concat("CfgGet_", $name, "_Api"), "current", "CfgGet", "current", $item ),
		local:itemFuncDecl( concat("CfgGet_", $name, "_TR069_Api"), "", "CfgGetTmp", "", $item ),
		local:itemFuncDecl( concat("CfgSet_", $name, "_Api"), " data, apply, save", "CfgSet", "data, apply, save", $item )
	)
};

declare function local:itemArrayDecl( $descType, $name, $item )
{
	if ($descType = "TABLE") then
	(
		local:itemFuncDecl( concat("CfgEntryGet_", $name, "_ByKeyApi"),  "key, retData, retLen, current", "CfgGetByKey", "retData, retLen, current, key", $item ),
		local:itemFuncDecl( concat("CfgEntryGet_", $name, "_TR069_ByKeyApi"), "key, retData, retLen", "CfgGetTmpByKey", "retData, retLen, key", $item ),
		local:itemFuncDecl( concat("CfgEntryGet_", $name, "_ByIndexApi"), "index, retData, retLen, current", "CfgGetByIndex", "retData, retLen, current, index", $item ),
		local:itemFuncDecl( concat("CfgEntrySet_", $name, "_Api"), "key, data, dataLen, apply, save", "CfgSetByKey", "data, dataLen, apply, save, key", $item )
	)
(:
	else if ($descType = "PORT") then
	(
		local:itemFuncDecl( concat("CfgGet_", $name, "_Api"), "port, retData, retLen, current", $item ),
		local:itemFuncDecl( concat("CfgGet_", $name, "_TR069_Api"), "port, retData, retLen", $item ),
		local:itemFuncDecl( concat("CfgSet_", $name, "_Api"), "port, data, datalen, apply, save", $item )
	)
:)
	else
	(
		local:itemFuncDecl( concat("CfgGet_", $name, "_Api"), "retData, retLen, current", "CfgGet", "retData, retLen, current", $item ),
		local:itemFuncDecl( concat("CfgGet_", $name, "_TR069_Api"), "retData, retLen", "CfgGetTmp", "retData, retLen", $item ),
		local:itemFuncDecl( concat("CfgSet_", $name, "_Api"), "data, dataLen, apply, save", "CfgSet", "data, dataLen, apply, save", $item )
	)
};

declare function local:itemDecl( $descType, $item )
{
	let $name := concat(local:format_struct_name( $item/../@Name ), '_', local:format_struct_name( $item/@Name ))
	let $type := local:itemType( $item )
	return switch ($item/@Type)
		case "YN"
		case "ABLE"
			return local:itemBoolDecl( $descType, $name, $item )
		case "U8"
		case "S8"
		case "U16"
		case "S16"
		case "IPV4"
		case "IPV4MASK"
		case "LIST"
		case "RELATION"
		case "U32"
		case "S32"
		case "FLOAT"
			return local:itemBasicDecl( $descType, $name, $item )
		case "MAC"
		case "STRING"
		case "FILESELECT"
		case "PASSWD"
			return local:itemArrayDecl( $descType, $name, $item )
		default return ''
};

declare function local:descDecls( $descs )
{
	for $desc in $descs
	return
	(
		concat( "/* ----- ", $desc/@Name ," ----- */" ),
		'',
		local:descStruct( $desc ),
		'',
		local:descDecl( $desc ),
		'',
		for $item in $desc/*
			return local:itemDecl( local:descType($desc), $item ),
		''
	)
};

declare function local:DescMcpDefine( $name, $type, $prefix, $bHasCount, $toMcpDataBody, $parseMcpDataBody )
{
	(
		local:toMcpDataDecl( $name, $type, $prefix, $bHasCount, $toMcpDataBody ),
		'',
		local:parseMcpDataDecl( $name, $type, $prefix, $parseMcpDataBody )
	)
};

declare function local:mcpAddItem( $item, $lenSize )
{
	concat( "mcpAdd", $item/@Type, "( pBuf, &amp;offset, ", $item/@id, ", pData-&gt;", local:itemStructName($item), ", ", string($lenSize), " );" )
};

declare function local:mcpType( $desc )
{
	(:
		'ADMIN': HEADER: id1[2]+count[2], TLV: index[2]+id2[2]+len[2]+data
		'LIST' : no HEADER,               TLV: count[2]+id2[2]+len[1]+data
		''     : HEADER: id1[2],          TLV:          id2[2]+len[2]+data
	:)

	if ($desc/name() = "AdminDesc") then
		"ADMIN"
	else if ($desc/name() = "DataListDesc") then
		"LIST"
	else
		''
};

declare function local:ident( $seq as xs:string* )
{
	for $s in $seq
	return concat( $tab, $s )
};

(: void toMcpDataApi( [OpU16 count,] $type * pData, OpU8 *pBuf, OpU16 *pOffset ) :)
declare function local:toMcpDataBody( $desc )
{
	let $mcpType := local:mcpType($desc)
	let $id := $desc/@id
	return
	local:ident( (
		"OpU16 offset = *pOffset;",
		'',
		if ($mcpType != 'LIST') then
		(
			concat("pBuf[offset++] = GET_U16_BYTE1(", $id, ");"),
			concat("pBuf[offset++] = GET_U16_BYTE2(", $id, ");")
		)
		else '',
		if ($mcpType = 'ADMIN') then
		(
			"pBuf[offset++] = 0;",
			"pBuf[offset++] = 0;",
			''
		)
		else '',
		for $item in $desc/*
		return
			if ($mcpType = 'ADMIN') then
			(
				"pBuf[offset++] = 0;",
				"pBuf[offset++] = 0;",
				local:mcpAddItem( $item, 2 )
			)
			else if ($mcpType = 'LIST') then
			(
				"pBuf[offset++] = GET_U16_BYTE1(count);",
				"pBuf[offset++] = GET_U16_BYTE2(count);",
				local:mcpAddItem( $item, 1 )
			)
			else
				local:mcpAddItem( $item, 2 ),
		'',
		"*pOffset = offset;"
	) )
};

declare function local:mcpGetItem( $toPtr, $item, $fromPtr )
{
	let $name := concat($toPtr, local:itemStructName($item))
	let $type := local:itemTypeName($item)
	return switch ($type)
		case "YN"
		case "ABLE"
		case "U8"
		case "S8"
		case "U16"
		case "S16"
		case "IPV4"
		case "IPV4MASK"
		case "LIST"
		case "RELATION"
		case "U32"
		case "S32"
		case "FLOAT"
			return concat( $name, " = MCP_GET_", $type, "(", $fromPtr, ");" )
		case "MAC"
			return concat( "MCP_GET_", $type, "(", $fromPtr, ", ", $name, ");" )
		case "STRING"
		case "FILESELECT"
		case "PASSWD"
			return concat( "MCP_GET_", $type, "(", $fromPtr, ", ", $name, ", dataLen);" )
		default return ''
};

(: void parseMcpDataApi( $type * pCfg, OpU16 id2, OpU8 *pData, OpU16 dataLen ) :)
declare function local:parseMcpDataBody( $desc )
{
	local:ident( (
		"if (dataLen == 0) return;",
		"switch(id2) {",
		for $item in $desc/*
			return
		(
			concat( "case ", $item/@id, ":" ),
			local:ident( (
				local:mcpGetItem( "pCfg-&gt;", $item, "pData" ),
				"break;"
			) )
		),
		"}"
	) )
};

declare function local:itemGetFunc( $getType, $dataType )
{
	let $type :=
		switch ($getType)
		case "TABLEBYKEY" return "ByKey"
		case "TABLEBYINDEX" return "ByIndex"
		case "TABLETMPBYKEY" return "TmpByKey"
		case "PORTTMP" return "Tmp"
		case "TMP" return "Tmp"
		default return	''
	return concat("ProvGet", $type, $dataType)
};

declare function local:itemGetArgs( $getType, $tagName, $descId, $itemId, $castType, $itemName )
{
	let $args := concat( $tagName, ", ", $descId, ", ", $itemId, ", ", $castType, "&amp;(", $itemName, "), sizeof (", $itemName, ")" )
	let $additions :=
		switch ($getType)
		case "TABLEBYKEY" return ", current, key"
		case "TABLEBYINDEX" return ", current, index"
		case "TABLETMPBYKEY" return ", key"
		case "PORTTMP" return ''
		case "TMP" return ''
		default return	", current"
	return concat($args, $additions)
};

declare function local:castType( $type )
{
	switch ($type)
	case "MAC"
		return "(OpU8 *)"
	case "STRING"
	case "FILESELECT"
	case "PASSWD"
		return "(OpS8 *)"
	default return ''
};

declare function local:itemGet( $getType, $desc, $toPtr, $item )
{
	let $func  := local:itemGetFunc( $getType, local:itemTypeName($item))
	let $tagId := local:tagNameEnum($desc/../name())
	let $castType := local:castType(local:itemTypeName($item))
	let $name  := concat($toPtr, local:itemStructName($item))
	let $args  := local:itemGetArgs( $getType, $tagId, $desc/@id, $item/@id, $castType, $name )
	return concat($func, "( ", $args, " );")
};

declare function local:getType( $getType, $desc )
{
	let $descType := local:descType( $desc )
	let $tmp := if ($getType = "TMP") then "TMP" else ''
	let $type :=
		if ($descType = "TABLE") then
			if ($getType = "BYINDEX") then
				"BYINDEX"
			else
				"BYKEY"
		else ''
	return concat($descType, $tmp, $type)
};

(: void CfgGet( $type * pData, [OpU16 (key|index|port),] OpBool current ) :)
(: void CfgGet_TR069_( $type * pData, [OpU16 (key|port)] ) :)

declare function local:descGetBody( $getType, $desc )
{
	let $getType := local:getType( $getType, $desc )
	return local:ident( (
		"memset(pData, '\0', sizeof (*pData));",
		for $item in $desc/*
			return local:itemGet( $getType, $desc, "pData-&gt;", $item )
	) )
};

declare function local:itemSet( $desc, $toPtr, $item )
{
	let $itemType := local:itemTypeName($item)
	let $descType := local:descType( $desc )
	let $tagId := local:tagNameEnum($desc/../name())
	let $castType := local:castType(local:itemTypeName($item))
	let $name  := concat($toPtr, local:itemStructName($item))
	return if ($descType = "TABLE") then
			concat( "rv = rv || ProvSetByKey", $itemType, "( ", $tagId, ", ",  $desc/@id, ", ", $item/@id, ", ",
				$castType, "&amp;(", $name, "), sizeof (", $name, "), apply, OpFalse, key );" )
(:
		else if ($destType = "PORT") then
:)
		else
			concat( "rv = rv || ProvSet", $itemType, "( ", $tagId, ", ",  $desc/@id, ", ", $item/@id, ", ",
				$castType, "&amp;(", $name, "), sizeof (", $name, "), apply, OpFalse );" )
};

(: OpS32 CfgSave( $type * pData, [OpU8 key, OpBool bAdd,|OpU16 port,] OpBool apply ) :)

declare function local:callTableAdd( $desc )
{
	let $tagId := local:tagNameEnum($desc/../name())
	return concat( "ProvisionDBTableAddRecordApi(", $tagId, ", ", $desc/@id, ", key, 1)" )
};

declare function local:descSaveBody( $desc )
{
	local:ident( (
		"int rv = 0;",
		if (local:descType($desc) = "TABLE") then
			concat( "if (bAdd &amp;&amp; ((key = ", local:callTableAdd($desc), ") <= 0)) return key;" )
		else '',
		for $item in $desc/*
			return local:itemSet( $desc, "pData-&gt;", $item ),
		"return rv;"
	) )
};

declare function local:descDefine( $desc )
{
	let $name := local:format_struct_name( $desc/@Name )
	let $type := local:structTypeName( $desc/@Name )
	let $descType := local:descType($desc)
	return
	(
		local:DescMcpDefine( $name, $type,
			local:mcpPrefix($desc), local:mcpHasCount($desc),
			local:toMcpDataBody( $desc ), local:parseMcpDataBody( $desc ) ),
		'',
		local:DescGetDecl( $descType, $name, $type,
			local:descGetBody('', $desc), local:descGetBody("TMP", $desc), local:descGetBody("BYINDEX", $desc) ),
		'',
		local:DescSaveDecl( $descType, $name, $type, local:descSaveBody( $desc ) ),
		''
	)
};

declare function local:descDefines( $descs )
{
	for $desc in $descs
	return
	(
		concat( "/* ----- ", $desc/@Name ," ----- */" ),
		'',
		local:descDefine( $desc ),
		''
	)
};

declare variable $all_names := (
	"PROVISION",
	"SETTINGTAB",
	"STATUS",
	"ALARMSA",
	"TEST",
	"DATALIST",
	"ADMINISTRATION"
);

declare variable $all_prefixes := (
	"PROV",
	"TAB",
	"STATUS",
	"ALARM",
	"TEST",
	"DATALIST",
	"ADMIN"
);

declare function local:main( $nodes, $outtype )
{
	switch ($outtype)
	case "enum" return
	(
		for $name at $pos in $all_names
		let $descs := $nodes[name()=$name]/*
		return 
			if (count( $descs ) != 0) then
				local:enumLevel1( $name, $all_prefixes[$pos], $descs )
			else '',
		for $name at $pos in $all_names
		let $descs := $nodes[name()=$name]/*
		return
			if (count( $descs ) != 0) then
				local:enumLevel2( $descs )
			else '',
		''
	)
	case "header" return
		for $name in $all_names
		let $descs := $nodes[name()=$name]/*
		return
			if (count( $descs ) != 0) then
			(
				concat( "/* ----- ", $name ," ----- */" ),
				'',
				local:descDecls( $descs ),
				''
			)
			else ''
	case "source" return
		for $name in $all_names
		let $descs := $nodes[name()=$name]/*
		return
			if (count( $descs ) != 0) then
			(
				concat( "/* ----- ", $name ," ----- */" ),
				'',
				local:descDefines( $descs ),
				''
			)
			else ''
	default return ""
};

declare variable $xmldoc external;
declare variable $outtype external;

local:main( (doc( $xmldoc )/*)[1]/*, $outtype )

