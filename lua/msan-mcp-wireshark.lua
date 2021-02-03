----------------------------------------
-- script-name: msan-mcp.lua
--

do

info("Wireshark version = " .. get_version())
info("Lua version = " .. _VERSION)

local mcp_str = "MSAN-MCP"

local mcp = Proto(mcp_str, "MSAN MCP Protocol")

local default_settings =
{
    heur_enabled = FALSE,
}

mcp.prefs.heur  = Pref.bool("Heuristic enabled", default_settings.heur_enabled,
                            "Whether heuristic dissection is enabled or not")

function mcp.prefs_changed()
    info(mcp_str .. " prefs_changed called")
end

local pf_mcphdr  = ProtoField.bytes("mcp.mcphdr", "MCP Header")
local pf_pt      = ProtoField.uint16("mcp.pt", "Payload Type", base.HEX)
local pf_group   = ProtoField.uint32("mcp.gk", "Group Key", base.HEX)
local pf_len     = ProtoField.uint16("mcp.len", "Data Length", base.HEX)

local pf_assignKeyHdr = ProtoField.bytes("mcp.assignkey", "Assign Key")
local pf_ak_opcode    = ProtoField.uint8("mcp.ak.opcode", "Opcode", base.HEX)
local pf_ak_shelf     = ProtoField.uint16("mcp.ak.shelf", "Shelf", base.HEX)
local pf_ak_slot      = ProtoField.uint16("mcp.ak.slot", "Slot", base.HEX)
local pf_ak_group     = ProtoField.uint32("mcp.ak.group", "Group Key", base.HEX)

local pf_ctrlhdr   = ProtoField.bytes("mcp.ctrlhdr", "Control Header")
local pf_ctrlflags = ProtoField.uint8("mcp.ctrlflags", "Control Flags", base.HEX)
local pf_cf_ack    = ProtoField.uint8("mcp.cf.ack", "Acknowledge", base.DEC, nil, 0x80)
local pf_cf_pri    = ProtoField.uint8("mcp.cf.pri", "Priority", base.DEC, nil, 0x60)
local pf_cf_shelf  = ProtoField.uint8("mcp.cf.shelf", "Shelf", base.DEC, nil, 0x0F)
local pf_slotno    = ProtoField.uint8("mcp.slot", "Slot Number")
local pf_seqno     = ProtoField.uint16("mcp.seq", "Sequence Number")

local pf_shelfno     = ProtoField.uint8("mcp.shelf", "Shelf Number")
local pf_ci_mcpver   = ProtoField.uint32("mcp.ci.mcpver", "MCP Version")
local pf_ci_category = ProtoField.uint32("mcp.ci.category", "Category")
local pf_ci_type     = ProtoField.uint32("mcp.ci.type", "Card Type")
local pf_ci_status   = ProtoField.uint16("mcp.ci.status", "Status", base.HEX)
local pf_ci_private  = ProtoField.uint16("mcp.ci.private", "Private Status", base.HEX)
local pf_ci_cfgver   = ProtoField.uint16("mcp.ci.cfgver", "Cfg Version")
local pf_ci_major    = ProtoField.uint8("mcp.ci.maj", "Major Version")
local pf_ci_minor    = ProtoField.uint8("mcp.ci.min", "Minor Version")
local pf_ci_rev      = ProtoField.uint8("mcp.ci.rev", "Revision")
local pf_ci_release   = ProtoField.uint8("mcp.ci.rel", "Release")
local pf_ci_hw_ver    = ProtoField.uint8("mcp.ci.hwver", "Hardware Version")
local pf_ci_fpga_ver  = ProtoField.uint8("mcp.ci.fpgaver", "FPGA Version")
local pf_ci_width     = ProtoField.uint8("mcp.ci.width", "Width")
local pf_ci_reserved  = ProtoField.bytes("mcp.ci.reserved", "Reserved")
local pf_ci_serialno  = ProtoField.string("mcp.ci.sn", "Serial Number")
local pf_ci_build     = ProtoField.string("mcp.ci.build", "Build")
local pf_ci_mac       = ProtoField.ether("mcp.ci.mac", "MAC Address")
local pf_ci_name      = ProtoField.string("mcp.ci.name", "Name")

local pf_cist_standby_act    = ProtoField.uint16("mcp.cist.stdbyact", "Priority", base.DEC, nil, 0x0001)
local pf_cist_auto_conf      = ProtoField.uint16("mcp.cist.autoconf", "Auto Configure", base.DEC, nil, 0x0002)
local pf_cist_auto_prov      = ProtoField.uint16("mcp.cist.autoprov", "Auto Provision", base.DEC, nil, 0x0004)
local pf_cist_remote_upgrade = ProtoField.uint16("mcp.cist.rmtupg", "Remote Upgrade", base.DEC, nil, 0x0008)
local pf_cist_audit          = ProtoField.uint16("mcp.cist.audit", "Audit", base.DEC, nil, 0x0010)
local pf_cist_statistics     = ProtoField.uint16("mcp.cist.stat", "Statistics", base.DEC, nil, 0x0020)
local pf_cist_ready          = ProtoField.uint16("mcp.cist.ready", "Ready", base.DEC, nil, 0x0040)

local pf_pv_port = ProtoField.uint16("mcp.pv.port", "Port")
local pf_pv_id1 = ProtoField.uint16("mcp.pv.id1", "ID1")
local pf_pv_id2 = ProtoField.uint16("mcp.pv.id2", "ID2")

local pf_pv_begin   = ProtoField.bool("mcp.pv.begin", "Begin")
local pf_pv_end     = ProtoField.bool("mcp.pv.end", "End")
local pf_pv_ipaddr  = ProtoField.ipv4("mcp.pv.ip", "IP Address")
local pf_pv_netmask = ProtoField.ipv4("mcp.pv.nm", "Subnet Mask")
local pf_pv_gateway = ProtoField.ipv4("mcp.pv.gw", "Gateway")
local pf_pv_vlan    = ProtoField.uint16("mcp.pv.vid", "VLAN ID")
local pf_pv_pri     = ProtoField.uint8("mcp.pv.pri", "VLAN Priority")
local pf_pv_tos     = ProtoField.uint8("mcp.pv.tos", "VLAN TOS")
local pf_pv_dns1    = ProtoField.ipv4("mcp.pv.dns1", "DNS1")
local pf_pv_dns2    = ProtoField.ipv4("mcp.pv.dns2", "DNS2")

local pf_pv_reg_rate = ProtoField.uint16("mcp.pv.regrate", "Register Rate")

local pf_tbl_str    = ProtoField.string("mcp.tbl.str", "String")

local pf_cdr_start  = ProtoField.uint32("mcp.cdr.start", "Start Time")
local pf_cdr_answer = ProtoField.uint32("mcp.cdr.answer", "Answer Time")
local pf_cdr_end    = ProtoField.uint32("mcp.cdr.answer", "End Time")
local pf_cdr_status = ProtoField.bytes("mcp.cdr.status", "Status")
local pf_cdr_flag   = ProtoField.uint8("mcp.cdr.flag", "Flag")
local pf_cdr_len    = ProtoField.uint8("mcp.cdr.len", "Length")
local pf_cdr_str    = ProtoField.string("mcp.cdr.str", "String")

local TLV_ARP_RSP   = 0x0002
local TLV_PROVISION = 0x000a
local TLV_TABLE_ADD = 0x0010
local TLV_POTS_CDR  = 0x2010

local tlv_types = {
  [0x0000] = "ACK",
  [0x0001] = "ARP Req",
  [TLV_ARP_RSP] = "ARP Rsp",
  [0x0003] = "Card Attendance",
  [0x0004] = "Transaction Req",
  [0x0804] = "Transaction Rsp",
  [0x0005] = "ASK Reply Req",
  [0x0805] = "ASK Reply Rsp",
  [0x0006] = "Echo Req",
  [0x0806] = "Echo Rsp",
  [0x0007] = "TFTP Upgrade Req",
  [0x0807] = "TFTP Upgrade Rsp",
  [TLV_PROVISION] = "Provision",
  [0x080A] = "Provision Req",
  [0x000B] = "Alarm Req",
  [0x080B] = "Alarm Rsp",
  [0x000C] = "Status Req",
  [0x080C] = "Status Rsp",
  [0x002C] = "Status V2 Req",
  [0x082C] = "Status V2 Rsp",
  [0x002D] = "Statistics Req",
  [0x082D] = "Statistics Rsp",
  [0x000E] = "PM Req",
  [0x080E] = "PM Rsp",
  [0x000F] = "Card PM Req",
  [0x080F] = "Card PM Rsp",
  [TLV_TABLE_ADD] = "Table Add",
  [0x0011] = "Table Delete",
  [0x0012] = "Card Test Req",
  [0x0812] = "Card Test Rsp",
  [0x0013] = "Lamp Test Req",
  [0x0813] = "Lamp Test Rsp",
  [0x0014] = "LED Report Req",
  [0x0814] = "LED Report Rsp",
  [0x0015] = "Data List Req",
  [0x0815] = "Data List Rsp",
  [0x0020] = "List File Req",
  [0x0820] = "List File Rsp",
  [0x0021] = "Get File Size Req",
  [0x0821] = "Get File Size Rsp",
  [0x0022] = "Upload File Req",
  [0x0822] = "Upload File Rsp",
  [0x0030] = "Retrieve Prov",
  [0x0830] = "Response Prov",
  [0x0031] = "Retrieve Table",
  [0x0831] = "Response Table",
  [0x0835] = "Set Ack Rsp",
  [0x0036] = "Alarm SA Req",
  [0x0836] = "Alarm SA Rsp",
  [0x0037] = "Test Req",
  [0x0837] = "Test Rsp",
  [0x0038] = "Admin Req",
  [0x0838] = "Admin Rsp",
  [0x0040] = "Reboot Req",
  [0x0840] = "Reboot Rsp",

  [0x2001] = "Offhook",
  [0x2002] = "Onhook",
  [0x2003] = "Dial Pulse",
  [0x2004] = "Ring on",
  [0x2005] = "Ring off",
  [0x2006] = "Line fail",
  [0x2007] = "Flashhook",
  [0x2008] = "Send Caller ID",
  [0x2009] = "Send Pulsemeter",
  [0x200A] = "Send Revbattery",
  [0x200B] = "Ring Patterns",
  [0x200D] = "Ring on with number",
  [0x200E] = "Allocate fullrate timeslot",
  [0x200F] = "Free fullrate timeslot",
  [TLV_POTS_CDR] = "CDR",
  [0x2011] = "Line status",
  [0x2012] = "RTP status",
  [0x4001] = "Media Create",
  [0x4002] = "Media Modify",
  [0x4003] = "Media Destroy",
  [0x4004] = "RTCP Req",
  [0x4005] = "Tone Start",
  [0x4006] = "Tone Stop",
  [0x5001] = "Fax Tone Detect",
  [0x5002] = "Fax Terminate",
  [0x5003] = "Prov Req",
  [0x5004] = "Prov Rsp",
  [0x6001] = "DSP Msg",
}


local pf_tlv       = ProtoField.bytes("mcp.tlv", "TLV")
local pf_tlv_type  = ProtoField.uint16("mcp.tlv.type", "Type", base.HEX, tlv_types)
local pf_tlv_len   = ProtoField.uint16("mcp.tlv.len", "Length", base.HEX)
local pf_tlv_value = ProtoField.bytes("mcp.tlv.value", "Value")

local pf_arp_rsp   = ProtoField.bytes("mcp.arprsp", "ARP RSP")
local pf_provision = ProtoField.bytes("mcp.prov", "Provision")
local pf_table_add = ProtoField.bytes("mcp.tbladd", "Table Add")
local pf_pots_cdr  = ProtoField.string("mcp.cdr", "CDR")

mcp.fields = {
  pf_mcphdr, pf_pt, pf_group, pf_len,
  pf_assignKeyHdr, pf_ak_opcode, pf_ak_shelf, pf_ak_slot, pf_ak_group,
  pf_ctrlhdr, pf_ctrlflags, pf_cf_ack, pf_cf_pri, pf_cf_shelf, pf_slotno, pf_seqno, pf_shelfno,
  pf_tlv, pf_tlv_type, pf_tlv_len, pf_tlv_value,
  pf_arp_rsp, pf_ci_mcpver, pf_ci_category, pf_ci_type, pf_ci_status, pf_ci_private,
  pf_ci_cfgver, pf_ci_major, pf_ci_minor, pf_ci_rev, pf_ci_release, pf_ci_hw_ver, pf_ci_fpga_ver,
  pf_ci_width, pf_ci_reserved, pf_ci_serialno, pf_ci_build, pf_ci_mac, pf_ci_name,
  pf_cist_standby_act, pf_cist_auto_conf, pf_cist_auto_prov, pf_cist_remote_upgrade,
  pf_cist_audit, pf_cist_statistics, pf_cist_ready,
  pf_provision, pf_pv_port, pf_pv_id1, pf_pv_id2, pf_pv_begin, pf_pv_end,
  pf_pv_ipaddr, pf_pv_netmask, pf_pv_gateway,
  pf_pv_vlan, pf_pv_pri, pf_pv_tos, pf_pv_dns1, pf_pv_dns2,
  pf_pv_reg_rate,
  pf_table_add, pf_tbl_str,
  pf_pots_cdr, pf_cdr_start, pf_cdr_answer, pf_cdr_end, pf_cdr_status, pf_cdr_flag, pf_cdr_len, pf_cdr_str,
}

function tlv_arp_rsp(tvbuf,pktinfo,root)
  if tvbuf:len() < 76 then
    return  -- invalid packet
  end

  root:set_text("TLV: ARP RSP")

  local arprsp = root:add(pf_arp_rsp, tvbuf(0, tvbuf:len()))
  arprsp:add(pf_shelfno, tvbuf(0, 1))
  arprsp:add(pf_slotno, tvbuf(1, 1))
  arprsp:add(pf_ci_mcpver, tvbuf(2, 4))
  arprsp:add(pf_ci_category, tvbuf(6, 4))
  arprsp:add(pf_ci_type, tvbuf(10, 2))
  local status = arprsp:add(pf_ci_status, tvbuf(12, 2))
  status:add(pf_cist_standby_act, tvbuf(12, 2))
  status:add(pf_cist_auto_conf, tvbuf(12, 2))
  status:add(pf_cist_auto_prov, tvbuf(12, 2))
  status:add(pf_cist_remote_upgrade, tvbuf(12, 2))
  status:add(pf_cist_audit, tvbuf(12, 2))
  status:add(pf_cist_statistics, tvbuf(12, 2))
  status:add(pf_cist_ready, tvbuf(12, 2))
  arprsp:add(pf_ci_private, tvbuf(14, 2))
  arprsp:add(pf_ci_cfgver, tvbuf(16, 2))
  arprsp:add(pf_ci_major, tvbuf(18, 1))
  arprsp:add(pf_ci_minor, tvbuf(19, 1))
  arprsp:add(pf_ci_rev, tvbuf(20, 1))
  arprsp:add(pf_ci_release, tvbuf(21, 1))
  arprsp:add(pf_ci_hw_ver, tvbuf(22, 1))
  arprsp:add(pf_ci_fpga_ver, tvbuf(23, 1))
  arprsp:add(pf_ci_width, tvbuf(24, 1))
  arprsp:add(pf_ci_reserved, tvbuf(25, 1))
  arprsp:add(pf_ci_serialno, tvbuf(26, 16))
  arprsp:add(pf_ci_build, tvbuf(42, 12))
  arprsp:add(pf_ci_mac, tvbuf(54, 6))
  arprsp:add(pf_ci_name, tvbuf(60, 16))
end

local tlv_provision_fields = {
  [1] = {
    [0] =   {len = 1, pf = pf_pv_begin,   name = "Begin (Net Config)"},
    [6] =   {len = 4, pf = pf_pv_ipaddr,  name = "Signal IP Address"},
    [7] =   {len = 4, pf = pf_pv_netmask, name = "Subnet Mask"},
    [8] =   {len = 4, pf = pf_pv_gateway, name = "Gateway"},
    [9] =   {len = 2, pf = pf_pv_vlan,    name = "VLAN ID"},
    [10] =  {len = 1, pf = pf_pv_pri,     name = "VLAN Priority"},
    [11] =  {len = 1, pf = pf_pv_tos,     name = "VLAN TOS"},
    [14] =  {len = 4, pf = pf_pv_dns1,    name = "DNS1"},
    [15] =  {len = 4, pf = pf_pv_dns2,    name = "DNS2"},
    [16] =  {len = 4, pf = pf_pv_ipaddr,  name = "Media IP Address"},
    [17] =  {len = 4, pf = pf_pv_netmask, name = "Subnet Mask"},
    [18] =  {len = 4, pf = pf_pv_gateway, name = "Gateway"},
    [19] =  {len = 2, pf = pf_pv_vlan,    name = "VLAN ID"},
    [20] =  {len = 1, pf = pf_pv_pri,     name = "VLAN Priority"},
    [21] =  {len = 1, pf = pf_pv_tos,     name = "VLAN TOS"},
    [998] = {len = 6, pf = pf_ci_mac,     name = "MAC Address"},
    [999] = {len = 1, pf = pf_pv_end,     name = "End"},
  },
  [2] = {
    [0] =   {len = 1, pf = pf_pv_begin,   name = "Begin (SIP Global Config)"},
    [1] =   {len = 4, pf = pf_tbl_str,    name = "Registrar Server 1"},
    [999] = {len = 1, pf = pf_pv_end,     name = "End"},
  },
  [3] = {
    [0] =   {len = 1, pf = pf_pv_begin,   name = "Begin (Telephony Config)"},
    [999] = {len = 1, pf = pf_pv_end,     name = "End"},
  },
  [4] = {
    [1] =   {len = 2, pf = pf_pv_reg_rate, name = "Register Rate"},
  }
}

local tlv_table_add_fields = {
  [1] = {
    [0] =   {len = 1, pf = pf_pv_begin,   name = "Begin (Voice Profile)"},
    [999] = {len = 1, pf = pf_pv_end,     name = "End"},
  },
  [2] = {
    [0] =   {len = 1, pf = pf_pv_begin,   name = "Begin (Line Profile)"},
    [999] = {len = 1, pf = pf_pv_end,     name = "End"},
  },
  [3] = {
    [0] =   {len = 1, pf = pf_pv_begin,   name = "Begin (Codec Profile)"},
    [999] = {len = 1, pf = pf_pv_end,     name = "End"},
  },
}

function tlv_provision(tvbuf,pktinfo,root)
  if tvbuf:len() <= 6 then
    return  -- invalid packet
  end

  local id1 = tvbuf(2, 2):uint()
  local id2 = tvbuf(4, 2):uint()
  local len = tvbuf:len() - 6

  local found = (tlv_provision_fields[id1][id2] ~= nil) and (tlv_provision_fields[id1][id2].len == len)
  local prov
  if found then
    root:set_text("TLV: Provision - " .. tlv_provision_fields[id1][id2].name)

    prov = root:add(tlv_provision_fields[id1][id2].pf, tvbuf(6, len))
  else
    prov = root:add(pf_provision, tvbuf(0, tvbuf:len()))
  end
  prov:add(pf_pv_port, tvbuf(0, 2))
  prov:add(pf_pv_id1, tvbuf(2, 2))
  prov:add(pf_pv_id2, tvbuf(4, 2))
  if not found then
    prov = prov:add(pf_tlv_value, tvbuf(6, len))
  end
end

function tlv_table_add(tvbuf,pktinfo,root)
  if tvbuf:len() <= 4 then
    return  -- invalid packet
  end

  local id1 = tvbuf(0, 2):uint()
  local id2 = tvbuf(2, 2):uint()
  local len = tvbuf:len() - 4

  local found = (tlv_table_add_fields[id1][id2] ~= nil) and (tlv_table_add_fields[id1][id2].len == len)
  local prov
  if found then
    root:set_text("TLV: Table Add - " .. tlv_table_add_fields[id1][id2].name)

    prov = root:add(tlv_table_add_fields[id1][id2].pf, tvbuf(4, len))
  else
    prov = root:add(pf_table_add, tvbuf(0, tvbuf:len()))
  end
  prov:add(pf_pv_id1, tvbuf(0, 2))
  prov:add(pf_pv_id2, tvbuf(2, 2))
  if not found then
    prov = prov:add(pf_tlv_value, tvbuf(4, len))
  end
end

function tlv_pots_cdr(tvbuf,pktinfo,root)
  if tvbuf:len() <= 18 then
    return  -- invalid packet
  end

  local len = tvbuf(17, 1):uint()
  if len > tvbuf:len() - 18 then
    return  -- invalid packet
  end

  root:set_text("TLV: POTS CDR")

  local cdr = root:add(pf_pots_cdr, tvbuf(18, len))
  cdr:add(pf_cdr_start,tvbuf(0, 4))
  cdr:add(pf_cdr_answer,tvbuf(4, 4))
  cdr:add(pf_cdr_end,tvbuf(8, 4))
  cdr:add(pf_cdr_status,tvbuf(12, 4))
  cdr:add(pf_cdr_flag,tvbuf(16, 1))
  cdr:add(pf_cdr_len,tvbuf(17, 1))
  cdr:add(pf_cdr_str,tvbuf(18, len))
end

local tlv_handlers = 
{
  [TLV_ARP_RSP]   = tlv_arp_rsp,
  [TLV_PROVISION] = tlv_provision,
  [TLV_TABLE_ADD] = tlv_table_add,
  [TLV_POTS_CDR]  = tlv_pots_cdr,
}

function mcp.dissector(tvbuf,pktinfo,root)
  -- set the protocol column to show our protocol name
  pktinfo.cols.protocol:set(mcp_str)

  local buflen = tvbuf:len()

  if buflen < 8 then
    return  -- no MCP header
  end

  local pt = tvbuf(0,2):uint()
  local group = tvbuf(2,4):bytes():tohex(true)
  local mcpdatalen = tvbuf(6,2):uint()

  pktinfo.cols.info:set("PT: " .. pt .. ", GK: 0x" .. group)

  if 8+mcpdatalen > buflen then
    return  -- invalid packet
  end

  -- MCP header

  local mcptree = root:add(mcp, tvbuf(0,8+mcpdatalen))

  local mcphdr = mcptree:add(pf_mcphdr, tvbuf(0,8))
  mcphdr:add(pf_pt, tvbuf(0,2))
  mcphdr:add(pf_group, tvbuf(2,4))
  mcphdr:add(pf_len, tvbuf(6,2))

  if pt == 1 then
    if (group == "00000000") and (mcpdatalen == 1) then
      -- Request Key

      pktinfo.cols.info:set("Request Key")
    elseif (group == "ffffffff") and (mcpdatalen == 9) then
      -- Assign key
      local assignKeyHdr = mcptree:add(pf_assignKeyHdr, tvbuf(8,9))
      assignKeyHdr:add(pf_ak_opcode, tvbuf(8,1))
      assignKeyHdr:add(pf_ak_shelf, tvbuf(9,2))
      assignKeyHdr:add(pf_ak_slot, tvbuf(11,2))
      assignKeyHdr:add(pf_ak_group, tvbuf(13,4))

      pktinfo.cols.info:set("Assign Key 0x" .. tvbuf(13,4):bytes():tohex(true))
    elseif mcpdatalen >= 4 then
      -- Control header

      local ctrlhdr = mcptree:add(pf_ctrlhdr, tvbuf(8,4))
      local ctrlflags = ctrlhdr:add(pf_ctrlflags, tvbuf(8,1))
      ctrlflags:add(pf_cf_ack, tvbuf(8,1))
      ctrlflags:add(pf_cf_pri, tvbuf(8,1))
      ctrlflags:add(pf_cf_shelf, tvbuf(8,1))
      ctrlhdr:add(pf_slotno, tvbuf(9,1))
      ctrlhdr:add(pf_seqno, tvbuf(10,2))

      -- TLVs

      local tlvofs = 12
      local tlvlen = mcpdatalen - 4
      local info_set = false
      
      while tlvlen > 4 do
        local tlvtype    = tvbuf(tlvofs, 2):uint()
        local tlvdatalen = tvbuf(tlvofs+2, 2):uint()
        if tlvdatalen <= tlvlen-4 then
          local tlv = mcptree:add(pf_tlv, tvbuf(tlvofs, 4+tlvdatalen))
          tlv:add(pf_tlv_type, tvbuf(tlvofs, 2))
          tlv:add(pf_tlv_len, tvbuf(tlvofs+2, 2))

          if tlv_handlers[tlvtype] ~= nil then
            local subtvb = TvbRange.tvb(tvbuf(tlvofs+4, tlvdatalen))
            tlv_handlers[tlvtype](subtvb, pktinfo, tlv)
          else
            tlv:add(pf_tlv_value, tvbuf(tlvofs+4, tlvdatalen))
          end

          if not info_set then
            pktinfo.cols.info:set(tlv_types[tlvtype])  -- set info as the first TLV type
            info_set = true
          end

          tlvofs = tlvofs + (4 + tlvdatalen)
          tlvlen = tlvlen - (4 + tlvdatalen)
        else
          break  -- stop parsing if encounter invalid TLV
        end
      end
    end
  end
end

DissectorTable.get("ethertype"):add(0x1219, mcp)

info(mcp_str .. " loaded!!!")

end
