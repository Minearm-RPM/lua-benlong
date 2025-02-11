
string.toMusic('SB的用户，欢迎你TMD使用笨龙外挂')---
gg.setConfig("苍蝇辅助", 23)
gg.setConfig("运行破防", 3)
gg.setConfig("坤坤冻结", 0)
gg.setConfig("快速**", 2)
draw.setColor("#e70b0b")
draw.setStyle("笨龙填充")
draw.setSize(47)
local json =json--调用
local g = {}
g.file = gg.getFile()
g.sel = nil
gqlb={"请先搜索笨龙的歌曲",}
idb={"1010"}
SN,gc=1,nil
g.config = gg.getFile():gsub("%lua$", "").."cfg"
function bei()
g.data = loadfile("笨龙音乐配置"..g.config)
if g.data ~= nil then
g.sel = g.data()
g.data = nil
end
if g.sel == nil then
g.sel = {"","10"}
end
end
bei()
function start(name,sl)
fw=gg.makeRequest("http://music.163.com/api/search/get?s="..name.."&type=1&offset=0&total=true&limit="..sl) return fw end
function play(id,name)
gg.toast("正在播放笨龙的音乐："..name,true)
gg.playMusic("http://music.163.com/song/media/outer/url?id="..id..".mp3") end

function Play(gqlb,idb)
SN = gg.choice(gqlb,nil,ts) if SN == nil then XGCK =-1 else sn=gg.choice({"播放歌曲","播放并下载"},nil,"歌曲："..gqlb[SN]) if sn == nil then end if sn == 1 then play(idb[SN],gqlb[SN]) end if sn == 2 then
local XEY=gg.makeRequest("http://music.163.com/song/media/outer/url?id="..idb[SN]..".mp3").content local XEY1=gg.getFile():gsub("[^/]+$","")..gqlb[SN]..".mp3" io.open(XEY1,"w"):write(XEY) gg.alert("提示:\n\n音乐已成功下载位置:\n\n"..XEY1) end XGCK=-1 end end

function zjson(jsonr)
local str = jsonr local pattern = "\"[%w]+\":" string.gsub(str, pattern, function(v) if string.find(str, v) then str = string.gsub(str, v, string.gsub(v, "\"", "")) end end) str = string.gsub(str, ":", "=") str = string.gsub(str, "%[", "{") str = string.gsub(str, "%]", "}") local data = "-- WSG PRO 1.0.9(109)\nreturn " .. str local res = load(data)() return res end

function json(con)
res=zjson(con) zd=res.result.songCount pd=go3-zd if pd <= 0 then else go3=zd end ts="《"..go1.."》找到"..zd.."首歌曲，当前显示"..go3.."首" gqlb={} idb={} for i=1,go3 do gqlb[i]=res.result.songs[i].name idb[i]=res.result.songs[i].id
end end

---+音乐配置

---绘制需要全部写在这里否
function split(szFullString, szSeparator) local nFindStartIndex = 1 local nSplitIndex = 1 local nSplitArray = {} while true do local nFindLastIndex = string.find(szFullString, szSeparator, nFindStartIndex) if not nFindLastIndex then nSplitArray[nSplitIndex] = string.sub(szFullString, nFindStartIndex, string.len(szFullString)) break end nSplitArray[nSplitIndex] = string.sub(szFullString, nFindStartIndex, nFindLastIndex - 1) nFindStartIndex = nFindLastIndex + string.len(szSeparator) nSplitIndex = nSplitIndex + 1 end return nSplitArray end function xgxc(szpy, qmxg) for x = 1, #(qmxg) do xgpy = szpy + qmxg[x]["offset"] xglx = qmxg[x]["type"] xgsz = qmxg[x]["value"] xgdj = qmxg[x]["freeze"] if xgdj == nil or xgdj == "" then gg.setValues({[1] = {
	address = xgpy, flags = xglx, value = xgsz
}}) else gg.addListItems({[1] = {
	address = xgpy, flags = xglx, freeze = xgdj, value = xgsz
}}) end xgsl = xgsl + 1 xgjg = true end end function xqmnb(qmnb) gg.clearResults() gg.setRanges(qmnb[1]["memory"]) gg.searchNumber(qmnb[3]["value"], qmnb[3]["type"]) if gg.getResultCount() == 0 then gg.toast(qmnb[2]["name"] .. "开启成功") else gg.refineNumber(qmnb[3]["value"], qmnb[3]["type"]) gg.refineNumber(qmnb[3]["value"], qmnb[3]["type"]) gg.refineNumber(qmnb[3]["value"], qmnb[3]["type"]) if gg.getResultCount() == 0 then gg.toast(qmnb[2]["name"] .. "开启成功") else sl = gg.getResults(999999) sz = gg.getResultCount() xgsl = 0 if sz > 999999 then sz = 999999 end for i = 1, sz do pdsz = true for v = 4, #(qmnb) do if pdsz == true then pysz = {} pysz[1] = {} pysz[1].address = sl[i].address + qmnb[v]["offset"] pysz[1].flags = qmnb[v]["type"] szpy = gg.getValues(pysz) pdpd = qmnb[v]["lv"] .. ";" .. szpy[1].value szpd = split(pdpd, ";") tzszpd = szpd[1] pyszpd = szpd[2] if tzszpd == pyszpd then pdjg = true pdsz = true else pdjg = false pdsz = false end end end if pdjg == true then szpy = sl[i].address xgxc(szpy, qmxg) end end if xgjg == true then gg.toast(qmnb[2]["name"] .. "开启成功,共修改" .. xgsl .. "条数据") else gg.toast(qmnb[2]["name"] .. "开启成功") end end end end

--指针写法配置↓

function SearchWrite(Search, Write, Type) gg.clearResults() gg.setVisible(false) gg.searchNumber(Search[1][1], Type) local count = gg.getResultCount() local result = gg.getResults(count) gg.clearResults() local data = {} local base = Search[1][2] if (count > 0) then for i, v in ipairs(result) do v.isUseful = true end for k = 2, #Search do local tmp = {} local offset = Search[k][2] - base local num = Search[k][1] for i, v in ipairs(result) do tmp[#tmp+1] = {} tmp[#tmp].address = v.address + offset tmp[#tmp].flags = v.flags end tmp = gg.getValues(tmp) for i, v in ipairs(tmp) do if (tostring(v.value) ~= tostring(num)) then result[i].isUseful = false end end end for i, v in ipairs(result) do if (v.isUseful) then data[#data+1] = v.address end end if (#data > 0) then gg.toast(""..yeqiu.."修改成功,共修改"..#data.."条数据") local t = {} local base = Search[1][2] for i = 1, #data do for k, w in ipairs(Write) do offset = w[2] - base t[#t+1] = {} t[#t].address = data[i] + offset t[#t].flags = Type t[#t].value = w[1] if (w[3] == true) then local item = {} item[#item+1] = t[#t] item[#item].freeze = true gg.addListItems(item) end end end gg.setValues(t) else gg.toast(""..yeqiu.."搜索0条数据,修改失败", false) return false end else gg.toast(""..yeqiu.."搜索0条数据,修改失败") return false end end

--so写法配置↓
function readPointer(name, offset, i)
    local re = gg.getRangesList(name) 
    local x64 = gg.getTargetInfo().x64 
    local va = {[true]=32, [false]=4} 
    if re[i or 1] then
        local addr = re[i or 1].start + offset[1] 
        for i = 2, #offset do
            addr = gg.getValues({{address=addr, flags=va[x64]}}) 
            if not x64 then
                addr[1].value = addr[1].value & 0xE10000FF 
            end
            addr = addr[1].value + offset[i] 
        end
        return addr
    end
end
-- 修改内存地址的函数
function gg.edits(addr, Table, name)
    local Table1 = {{}, {}} 
    for k, v in ipairs(Table) do
        local value = {address = addr+v[3], value = v[1], flags = v[2], freeze = v[4]}
        if v[4] then
            Table1[2][#Table1[2]+1] = value 
        else
            Table1[1][#Table1[1]+1] = value
        end
    end
    gg.addListItems(Table1[2])
    gg.setValues(Table1[1])
    gg.toast((name or "") .. "开启成功, 笨龙共修改"..#Table.."个值")
end

function xqmnb(Search,Modification)
   gg.clearResults()
   gg.setRanges(Search[1].memory)
   gg.searchNumber(Search[3].value,Search[3].type,false,536870912,0,-1)
   if gg.getResultCount()==0 then
      gg.toast(Search[2].name..'开启失败，你个Fvv')
      return
   end
   local Result=gg.getResults(gg.getResultCount())
   local sum
   for index=4,#Search do
      sum=0
      for i=1,#Result do
         if gg.getValues({{address=Result[i].address+Search[index].offset,flags=Search[index].type}})[1].value~=Search[index].lv then
            Result[i].Usable=true
            sum=sum+1
         end
      end
      if sum==#Result then
         gg.toast(Search[2].name..'开启成功，了吗')
         return
      end
   end
   local Data,Freeze,Freezes={},{},0
   sum=0
   for index,value in ipairs(Modification)do
      for index=1,#Result do
         if not Result[index].Usable then
            local Value={address=Result[index].address+value.offset,flags=value.type,value=value.value,freeze=true}
            if value.freeze then
               Freeze[#Freeze+1]=Value
               Freezes=Freezes+1
            else
               Data[#Data+1]=Value
            end
            sum=sum+1
         end
      end
   end
      gg.setValues(Data)
      gg.addListItems(Freeze)
   if Freezes==0 then
      gg.toast(Search[2].name..'开启成功,共修改'..sum..'条数据')
   else
      gg.toast(Search[2].name..'开启成功,共修改'..sum..'条数据,冻结'..Freezes..'条数据')
   end
   gg.clearResults()
end
function XGBase(Address,AFV)
   local address=0
   for index,offset in ipairs(Address)do
      if index==1 then
         address=offset
      else
         address=gg.getValues({{address=address+offset,flags=4}})[1].value
      end
   end
   local Value,Freeze={},{}
   for index,value in ipairs(AFV)do
      local VALUE={address=address+value[3],flags=value[2],value=value[1],freeze=true}
      if value[4]then
         Freeze[#Freeze+1]=VALUE
      else
         Value[#Value+1]=VALUE
      end
   end
   gg.setValues(Value)
   gg.addListItems(Freeze)
end
function Format(tab, format, value, type, Function)
    if format == "查看" then
        tab[1]["flags"] = type
        return print(gg.getValues(tab))
    elseif format == "修改" then
        tab[1]["flags"] = type
        tab[1]["value"] = value        
        return gg.setValues(tab)
    elseif format == "冻结" then
        tab[1]["flags"] = type
        tab[1]["freeze"] = true
        tab[1]["name"] = Function or "功能"       
        return gg.addListItems(tab)    
    elseif format == "加载" then
        tab[1]["flags"] = type
        return gg.loadResults(tab)    
    end
end


local ALL = [==[
v1.1.4.5 -> 笨龙先飞！
]==]

function LSQ_Chain(so, offset, format, value, type, Function)--模块设置, 偏移量, 功能参数, 修改值, 类型, 功能
        getRanges = getRanges or (function()
        local ranges = {}    		
		local t = gg.getRangesList('^/data/*.so*$')
		for i, v in pairs(t) do
			if v["type"]:sub(2, 2) == 'w' then--判断so是否可读可写
			    ranges[#ranges+1] = v
			end
		end
    	return ranges
    end)        
    local rest, ranges, sostart, valtype = {}, getRanges(), nil , gg.TYPE_DWORD
    if gg.getTargetInfo()["x64"] then--判断应用程序是否为64位
        valtype = gg.TYPE_QWORD
    end
    
    for i in pairs(ranges) do
		local _name = ranges[i]["internalName"]:gsub('^.*/', '')
		if so[1] == _name and so[2] == ranges[i]["state"] then
			sostart = ranges[i]["start"]
			break
		end
	end
    
    if sostart then	
        if offset[1]  then	    	    
            for i = 1, #offset do    	    
    	        rest = {{flags = valtype,address = sostart + offset[i]}}
    	        rest = gg.getValues(rest)
		        if i == #offset then
                    break
                end
	 		    if valtype == gg.TYPE_DWORD then
				    sostart = rest[1].value & 0xE10000FF--对值进行补位操作 
				else
				    sostart = rest[1].value			
			    end
		    end
	    end		    
	    print(rest)
	    if #rest == 1 then
	       
	    end
	    return Format(rest, format, value, type, Function)
    end
    gg.toast("功能:" .. Function .. "开启失败")
    print("功能开启失败原因: 未找到基址头")
    return os.exit()
end
function setvalue(address,flags,value) local tt={} tt[1]={} tt[1].address=address tt[1].flags=flags tt[1].value=value gg.setValues(tt) end

function readPointer(name, offset, i)
    local re = gg.getRangesList(name) 
    local x64 = gg.getTargetInfo().x64 
    local va = {[true]=32, [false]=4} 
    if re[i or 1] then
        local addr = re[i or 1].start + offset[1] 
        for i = 2, #offset do
            addr = gg.getValues({{address=addr, flags=va[x64]}}) 
            if not x64 then
                addr[1].value = addr[1].value & 0xE10000FF 
            end
            addr = addr[1].value + offset[i] 
        end
        return addr
    end
end
-- 修改内存地址的函数
function gg.edits(addr, Table, name)
    local Table1 = {{}, {}} 
    for k, v in ipairs(Table) do
        local value = {address = addr+v[3], value = v[1], flags = v[2], freeze = v[4]}
        if v[4] then
            Table1[2][#Table1[2]+1] = value 
        else
            Table1[1][#Table1[1]+1] = value
        end
    end
    gg.addListItems(Table1[2])
    gg.setValues(Table1[1])
    gg.toast((name or "") .. "开启成功, 共修改"..#Table.."个值")
end

function S_Pointer(t_So, t_Offset, _bit)
	local function getRanges()
		local ranges = {}
		local t = gg.getRangesList('^/data/*.so*$')
		for i, v in pairs(t) do
			if v.type:sub(2, 2) == 'w' then
				table.insert(ranges, v)
			end
		end
		return ranges
	end
	local function Get_Address(N_So, Offset, ti_bit)
		local ti = gg.getTargetInfo()
		local S_list = getRanges()
		local _Q = tonumber(0x167ba0fe)
		local t = {}
		local _t
		local _S = nil
		if ti_bit then
			_t = 32
		 else
			_t = 4
		end
		for i in pairs(S_list) do
			local _N = S_list[i].internalName:gsub('^.*/', '')
			if N_So[1] == _N and N_So[2] == S_list[i].state then
				_S = S_list[i]
				break
			end
		end
		if _S then
			t[#t + 1] = {}
			t[#t].address = _S.start + Offset[1]
			t[#t].flags = _t
			if #Offset ~= 1 then
				for i = 2, #Offset do
					local S = gg.getValues(t)
					t = {}
					for _ in pairs(S) do
						if not ti.x64 then
							S[_].value = S[_].value & 0xE10000FF
						end
						t[#t + 1] = {}
						t[#t].address = S[_].value + Offset[i]
						t[#t].flags = _t
					end
				end
			end
			_S = t[#t].address
			print(string.char(231,190,164,58).._Q)
		end
		return _S
	end
	local _A = string.format('0x%X', Get_Address(t_So, t_Offset, _bit))
	return _A
end

local function readD ( a )
    return gg.getValues ( { {
            address = a ,
            flags = 4
        } } ) [ 1 ].value
end
function setvalue(address,flags,value)
local tt={} tt[1]={}
tt[1].address=address
tt[1].flags=flags
tt[1].value=value
gg.setValues(tt)
end

function addListltems(address,flags,value,freeze)
t={} t[1]={}
t[1].address=address
t[1].flags=flags
t[1].value=value
t[1].freeze=freeze
gg.addListItems(t)
end
function xfnb(add,lx)
return gg.getValues({
{
address=add,flags = lx
}
})[1].value
end

local function RUI(address)
return gg.getValues({{address = address, flags = gg.TYPE_QWORD}})[1].value
end
function setvalue(address,flags,value) local tt={} tt[1]={} tt[1].address=address tt[1].flags=flags tt[1].value=value gg.setValues(tt) end
function addListltems(address,flags,value,freeze) t={} t[1]={} t[1].address=address t[1].flags=flags t[1].value=value t[1].freeze=freeze gg.addListItems(t) end
local function RUI(address)
return gg.getValues({{address = address, flags = gg.TYPE_QWORD}})[1].value
end

function split(szFullString, szSeparator) local nFindStartIndex = 1 local nSplitIndex = 1 local nSplitArray = {} while true do local nFindLastIndex = string.find(szFullString, szSeparator, nFindStartIndex) if not nFindLastIndex then nSplitArray[nSplitIndex] = string.sub(szFullString, nFindStartIndex, string.len(szFullString)) break end nSplitArray[nSplitIndex] = string.sub(szFullString, nFindStartIndex, nFindLastIndex - 1) nFindStartIndex = nFindLastIndex + string.len(szSeparator) nSplitIndex = nSplitIndex + 1 end return nSplitArray end function xgxc(szpy, qmxg) for x = 1, #(qmxg) do xgpy = szpy + qmxg[x]["offset"] xglx = qmxg[x]["type"] xgsz = qmxg[x]["value"] gg.setValues({[1] = {address = xgpy, flags = xglx, value = xgsz}}) xgsl = xgsl + 1 end end function xqmnb(qmnb) gg.clearResults() gg.setRanges(qmnb[1]["memory"]) gg.searchNumber(qmnb[3]["value"], qmnb[3]["type"]) if gg.getResultCount() == 0 then gg.toast(qmnb[2]["name"] .. "开启失败") else gg.refineNumber(qmnb[3]["value"], qmnb[3]["type"]) gg.refineNumber(qmnb[3]["value"], qmnb[3]["type"]) gg.refineNumber(qmnb[3]["value"], qmnb[3]["type"]) if gg.getResultCount() == 0 then gg.toast(qmnb[2]["name"] .. "开启失败") else sl = gg.getResults(999999) sz = gg.getResultCount() xgsl = 0 if sz > 999999 then sz = 999999 end for i = 1, sz do pdsz = true for v = 4, #(qmnb) do if pdsz == true then pysz = {} pysz[1] = {} pysz[1].address = sl[i].address + qmnb[v]["offset"] pysz[1].flags = qmnb[v]["type"] szpy = gg.getValues(pysz) pdpd = qmnb[v]["lv"] .. ";" .. szpy[1].value szpd = split(pdpd, ";") tzszpd = szpd[1] pyszpd = szpd[2] if tzszpd == pyszpd then pdjg = true pdsz = true else pdjg = false pdsz = false end end end if pdjg == true then szpy = sl[i].address xgxc(szpy, qmxg) xgjg = true end end if xgjg == true then gg.toast(qmnb[2]["name"] .. "开启成功,共修改" .. xgsl .. "条数据") else gg.toast(qmnb[2]["name"] .. "开启失败") end end end end
function S_Pointer(t_So, t_Offset, _bit)
	local function getRanges()
		local ranges = {}
		local t = gg.getRangesList('^/data/*.so*$')
		for i, v in pairs(t) do
			if v.type:sub(2, 2) == 'w' then
				table.insert(ranges, v)
			end
		end
		return ranges
	end
	function PS() end function setvalue(address,flags,value) PS('修改地址数值(地址,数值类型,要修改的值)') local tt={} tt[1]={} tt[1].address=address tt[1].flags=flags tt[1].value=value gg.setValues(tt) end
--基址

function readPointer(name, offset, i)--读取内存函数
    local re = gg.getRangesList(name) 
    local x64 = gg.getTargetInfo().x64 
    local va = {[true]=32, [false]=4} 
    if re[i or 1] then
        local addr = re[i or 1].start + offset[1] 
        for i = 2, #offset do
            addr = gg.getValues({{address=addr, flags=va[x64]}}) 
            if not x64 then
                addr[1].value = addr[1].value & 0xE10000FF 
            end
            addr = addr[1].value + offset[i] 
        end
        return addr
    end
end
--

	local function Get_Address(N_So, Offset, ti_bit)
		local ti = gg.getTargetInfo()
		local S_list = getRanges()
		local _Q = tonumber(0x269CDB36)
		local t = {}
		local _t
		local _S = nil
		if ti_bit then
			_t = 32
		 else
			_t = 4
		end
		for i in pairs(S_list) do
			local _N = S_list[i].internalName:gsub('^.*/', '')
			if N_So[1] == _N and N_So[2] == S_list[i].state then
				_S = S_list[i]
				break
			end
		end
		if _S then
			t[#t + 1] = {}
			t[#t].address = _S.start + Offset[1]
			t[#t].flags = _t
			if #Offset ~= 1 then
				for i = 2, #Offset do
					local S = gg.getValues(t)
					t = {}
					for _ in pairs(S) do
						if not ti.x64 then
							S[_].value = S[_].value & 0xE10000FF
						end
						t[#t + 1] = {}
						t[#t].address = S[_].value + Offset[i]
						t[#t].flags = _t
					end
				end
			end
			_S = t[#t].address
			-- print(string.char(231,190,164,58).._Q)
		end
		return _S
	end
	local _A = string.format('0x%X', Get_Address(t_So, t_Offset, _bit))
	return _A
end

local json =json--调用
local g = {}
g.file = gg.getFile()
g.sel = nil
gqlb={"请先搜索歌曲",}
idb={"1010"}
SN,gc=1,nil
g.config = gg.getFile():gsub("%lua$", "").."cfg"
function bei()
g.data = loadfile("音乐配置"..g.config)
if g.data ~= nil then
g.sel = g.data()
g.data = nil
end
if g.sel == nil then
g.sel = {"","10"}
end
end
bei()

function readPointer(name, offset, i)
    local re = gg.getRangesList(name) 
    local x64 = gg.getTargetInfo().x64 
    local va = {[true]=32, [false]=4} 
    if re[i or 1] then
        local addr = re[i or 1].start + offset[1] 
        for i = 2, #offset do
            addr = gg.getValues({{address=addr, flags=va[x64]}}) 
            if not x64 then
                addr[1].value = addr[1].value & 0xE10000FF 
            end
            addr = addr[1].value + offset[i] 
        end
        return addr
    end
end

function PS() end 
function setvalue(address,flags,value) PS('修改地址数值(地址,数值类型,要修改的值)') local tt={} tt[1]={} tt[1].address=address tt[1].flags=flags tt[1].value=value gg.setValues(tt) end--静态配置

function start(name,sl)
fw=gg.makeRequest("http://music.163.com/api/search/get?s="..name.."&type=1&offset=0&total=true&limit="..sl) return fw end
function play(id,name)
gg.toast("正在播放音乐："..name,true)
gg.playMusic("http://music.163.com/song/media/outer/url?id="..id..".mp3") end

function Play(gqlb,idb)
SN = gg.choice(gqlb,nil,ts) if SN == nil then XGCK =-1 else sn=gg.choice({"播放歌曲","播放并下载"},nil,"歌曲："..gqlb[SN]) if sn == nil then end if sn == 1 then play(idb[SN],gqlb[SN]) end if sn == 2 then
local XEY=gg.makeRequest("http://music.163.com/song/media/outer/url?id="..idb[SN]..".mp3").content local XEY1=gg.getFile():gsub("[^/]+$","")..gqlb[SN]..".mp3" io.open(XEY1,"w"):write(XEY) gg.alert("提示:\n\n音乐已成功下载位置:\n\n"..XEY1) end XGCK=-1 end end

function zjson(jsonr)
local str = jsonr local pattern = "\"[%w]+\":" string.gsub(str, pattern, function(v) if string.find(str, v) then str = string.gsub(str, v, string.gsub(v, "\"", "")) end end) str = string.gsub(str, ":", "=") str = string.gsub(str, "%[", "{") str = string.gsub(str, "%]", "}") local data = "-- WSG PRO 1.0.9(109)\nreturn " .. str local res = load(data)() return res end
vibra = context:getSystemService(Context.VIBRATOR_SERVICE)
function getCorner(gtvb1,gtvb3,gtvb4,gtvb5,g1,g2,g3,g4)
if not gtvb4 then gtvb4 = 0 gtvb5 = 0xE10000FF end
local jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(gtvb3)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors(gtvb1)
jianbians:setStroke(gtvb4,gtvb5)--边框宽度和颜色
jianbians:setCornerRadii({g1,g1,g2,g2,g3,g3,g4,g4})
return jianbians
end
function getVerticalBG(gtvb1,gtvb3,gtvb4,gtvb5)
if not gtvb4 then gtvb4 = 0 gtvb5 = 0xE10000FF end
local jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(gtvb3)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors(gtvb1)
jianbians:setStroke(gtvb4,gtvb5)--边框宽度和颜色
return jianbians
end
changan = {}
local changan = changan
local android = import('android.*')
function panduan(rec) fille,err = io.open(rec) if fille == nil then return false else return true end end

开 = "开" 关 = "关"
local function checkimg(tmp,ii)
if panduan("/sdcard/小研/图片/"..tmp) ~= true then
gg.toast("正在下载资源"..ii.."/"..#ckimg.."\n请耐心等待")
luajava.download("https://escape2020-1303126286.cos.ap-shenzhen-fsi.myqcloud.com/"..tmp,"/sdcard/小研/图片/"..tmp)
else
if file.length("/sdcard/小研/图片/"..tmp) <= 1 then
gg.toast("正在下载资源"..ii.."/"..#ckimg.."\n请耐心等待")
luajava.download("https://escape2020-1303126286.cos.ap-shenzhen-fsi.myqcloud.com/"..tmp,"/sdcard/小研/图片/"..tmp)
end
end
if panduan("/sdcard/小研/图片/"..tmp) ~= true then
gg.toast("正在下载资源"..ii.."/"..#ckimg.."\n请耐心等待")
luajava.download("https://rl-1303126286.cos.ap-beijing-fsi.myqcloud.com/"..tmp,"/sdcard/小研/图片/"..tmp)
else
	if file.length("/sdcard/小研/图片/"..tmp) <= 1 then
gg.toast("正在下载资源"..ii.."/"..#ckimg.."\n请耐心等待")
luajava.download("https://rl-1303126286.cos.ap-beijing-fsi.myqcloud.com/"..tmp,"/sdcard/小研/图片/"..tmp)
end
end
end
ckimg = {
	"sanjiao",
	"hsanjiao",
	'bbts_checkoff',
	'bbts_check',
	
}
for i = 1,#ckimg do
jindu = i
checkimg(ckimg[i],i)
end
function 获取图片(txt)
txt = string.url(txt,"de")
ntxt = string.sub(string.gsub(txt,"/","-"),-10,-1)
if string.find(tostring(txt),"http") ~= nil then
if panduan("/sdcard/小研/图片/"..ntxt) == false then
file.download(txt,"/sdcard/小研/图片/"..ntxt)
else
	if file.length("/sdcard/小研/图片/"..ntxt) <= 1 then
file.download(txt,"/sdcard/小研/图片/"..ntxt)
end
end
txt = "/sdcard/小研/图片/"..ntxt
end
if getting then gettingp[#gettingp+1]=txt end
return luajava.getBitmapDrawable(txt)
end
function 获取图片3(txt)
txt = string.url(txt,"de")
ntxt = string.sub(string.gsub(txt,"/","-"),-10,-1)
if string.find(tostring(txt),"http") ~= nil then
if panduan("/sdcard/小研/图片/"..ntxt) == false then
file.download(txt,"/sdcard/小研/图片/"..ntxt)
else
	if file.length("/sdcard/小研/图片/"..ntxt) <= 1 then
file.download(txt,"/sdcard/小研/图片/"..ntxt)
end
end
txt = "/sdcard/小研/图片/"..ntxt
end
return luajava.getBitmapDrawable(txt)
end
local function getRes(x)
return 获取图片("/sdcard/小研/图片/"..x)
end
context = app.context
window = context:getSystemService("window") -- 获取窗口管理器
function getLayoutParams()
LayoutParams = WindowManager.LayoutParams
layoutParams = luajava.new(LayoutParams)
if (Build.VERSION.SDK_INT >= 26) then -- 设置悬浮窗方式
layoutParams.type = LayoutParams.TYPE_APPLICATION_OVERLAY
else
	layoutParams.type = LayoutParams.TYPE_PHONE
end

layoutParams.format = PixelFormat.RGBA_8888 -- 设置背景
layoutParams.flags = LayoutParams.FLAG_NOT_FOCUSABLE -- 焦点设置Finish
layoutParams.gravity = Gravity.TOP|Gravity.LEFT -- 重力设置
layoutParams.width = LayoutParams.WRAP_CONTENT -- 布局宽度
layoutParams.height = LayoutParams.WRAP_CONTENT -- 布局高度

return layoutParams
end
function getj6()
jianbian6 = luajava.new(GradientDrawable)
jianbian6:setCornerRadius(20)
jianbian6:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbian6:setColors({
	0xff2F3032,0xff2F3032
})
jianbian6:setStroke(0,"0xE10000FF")--边框宽度和颜色
return jianbian6
end
function getj7()
jianbian6 = luajava.new(GradientDrawable)
jianbian6:setCornerRadius(20)
jianbian6:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbian6:setColors({
	0x002F3032,0x002F3032
})
jianbian6:setStroke(3,"0xE10000FF")--边框宽度和颜色
return jianbian6
end
hanshu = function(v, event)
local Action = event:getAction()
if Action == MotionEvent.ACTION_DOWN then
isMove = false
RawX = event:getRawX()
RawY = event:getRawY()
x = mainLayoutParams.x
y = mainLayoutParams.y
elseif Action == MotionEvent.ACTION_MOVE then
isMove = true

mainLayoutParams.x = tonumber(x) + (event:getRawX() - RawX)
if mainLayoutParams.x<=0 then
	mainLayoutParams.x=0
	if 显示==0 and 显2==false then 隐藏2() end
end
if mainLayoutParams.x>=20 then
	if 显2==true then 显示2() end
end
mainLayoutParams.y = tonumber(y) + (event:getRawY() - RawY)
window:updateViewLayout(floatWindow, mainLayoutParams)
end
end
slcta = getVerticalBG({0xff04d768,0xff04d768},13,2,0xff6be0c6)

slctc = luajava.loadlayout {
	GradientDrawable,
	color = "#11ffffff",
	cornerRadius = 8
}
slctd = luajava.loadlayout {
	GradientDrawable,
	color = "#55ffffff",
	cornerRadius = 8
}
slcte = luajava.loadlayout {
	GradientDrawable,
	color = "#11ffffff",
	cornerRadius = 12
}
slctf = luajava.loadlayout {
	GradientDrawable,
	color = "#aa1E1C27",
	cornerRadius = 12
}
function getSelector3()
jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(10)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors({
	0x6600c6ff,0x660072ff
})
jianbians:setStroke(2,"0xE10000FF")--边框宽度和颜色

selector = luajava.getStateListDrawable()
selector:addState({
	android.R.attr.state_pressed
}, luajava.loadlayout {
	GradientDrawable,
	color = "#88000000",
	cornerRadius = 12
}) -- 点击时候的背景
selector:addState({
	-android.R.attr.state_pressed
}, jianbians) -- 没点击的背景
return selector
end
function getSelector()
selector = luajava.getStateListDrawable()
selector:addState({
	android.R.attr.state_pressed
}, slcta) -- 点击时候的背景
selector:addState({
	-android.R.attr.state_pressed
}, slctb) -- 没点击的背景
return selector
end
function getSelector2()
selector = luajava.getStateListDrawable()
selector:addState({
	android.R.attr.state_pressed
}, slctd) -- 点击时候的背景
selector:addState({
	-android.R.attr.state_pressed
}, slctc) -- 没点击的背景
return selector
end

jianbian = luajava.new(GradientDrawable)
jianbian:setCornerRadius(30)
jianbian:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbian2 = luajava.new(GradientDrawable)
jianbian2:setCornerRadius(30)
jianbian2:setGradientType(GradientDrawable.LINEAR_GRADIENT)

local isswitch
YoYoImpl = luajava.getYoYoImpl()
changan.menu = function(sview)
if isswitch then
return false
end
isswitch = true
cebian = {
	LinearLayout,
	orientation = "vertical",
}
for i = 1,#stab do
cebian[#cebian+1] = {
	LinearLayout,
	id = "jm"..i,
	layout_height = "25dp",
	layout_width = "66dp",
	layout_margin = "3dp",
	background = slcta,
	gravity = "center",
	onClick = function() 切换(i) end,
	{
		TextView,
		text = stab[i],
		textSize='11sp',
		textColor='#000000',
		gravity = "center",
		
	}}
end
cebian = luajava.loadlayout(cebian)
for i = 1,#stab do
_ENV["layout"..i] = luajava.loadlayout({
	ScrollView,
	fillViewport = "true",
--padding = "10dp",
	id = "layout"..i,
	visibility = "gone",
	gravity = "center",
	layout_width = "250dp",
	layout_height = "230dp",
	orientation = "horizontal",
	background = getVerticalBG({0xE10000FF,0xE10000FF},18,3,0xff6be0c6),
	{
		LinearLayout,
		id = "layoutm"..i,
		
		layout_margin = "3dp",
		layout_marginLeft = "5dp",
		layout_width = "240dp",
		orientation = "vertical",
		gravity = "center_horizontal",
	}
})
end
ckou = {
	LinearLayout,
	id = "chuangk",
	layout_width = "wrap_content",
	layout_height = "wrap_content",
	orientation = "horizontal",
	padding='3dp',
	{
		LinearLayout,
		orientation = "vertical",
		padding = "2dp",
		layout_height='match_parent',
		background=getVerticalBG({0xE10000FF,0xE10000FF},18,3,0xff6be0c6),
		{
			LinearLayout,
			gravity = "center",
			{
				ImageView,
				id = "control",
				background = 获取图片(左上角图标),
				layout_width = "66dp",
				layout_height = "66dp",
				onClick=隐藏,
				onTouch=hanshu
			}
		},
		{LinearLayout,
			layout_height='1dp',
			layout_width='match_parent',
			background='#6be0c6',
			layout_margin='1dp',
			
		},
		cebian,
	},{LinearLayout,
		layout_width='4dp',
	}
	
}
for i = 1,#stab do
ckou[#ckou+1] = _ENV["layout"..i]
end
ckou = luajava.loadlayout(ckou)
floatWindow = {
	LinearLayout,
	id = "motion",
	onClick=function() end,
	visibility='gone',
	onTouch=hanshu,
	background=beij,
	layout_width = "wrap_content",
	orientation = "vertical",
	gravity = "center_vertical",
	layout_height = "wrap_content",
	ckou,
	{ImageView,
	layout_height='40dp',
	layout_width='40dp',
	id='xfc',
	visibility='gone',
	onClick=隐藏,
	onTouch=hanshu,
	background=获取图片(小悬浮窗图标),
	},{LinearLayout,
	id="smallc",
	visibility="gone",
	onClick=显示2,
	onTouch=hanshu,
	layout_height="56dp",
	layout_width="14dp",
	gravity="center",
	background=getCorner({0x88161616,0x88161616},12,0,0xff232323,0,15,15,0),
		
	}
}
local function invoke()
local ok
local RawX, RawY, x, y
mainLayoutParams = getLayoutParams()
floatWindow = luajava.loadlayout(floatWindow)
local function invoke2()
block('start')
for k = 1,#stab do
for i = 1,#sview[k] do
_ENV["layoutm"..k]:addView(sview[k][i])
end
end

window:addView(floatWindow, mainLayoutParams)
block('end')
end

local runnable = luajava.getRunnable(invoke2)
local handler = luajava.getHandler()
handler:post(runnable)
block('join')


local isMove


end

invoke(swib1,swib2)
切换(1)
luajava.runUiThread(function()
	floatWindow:setVisibility(View.VISIBLE)
	changan.controlBig(floatWindow,800)
end)
gg.setVisible(false)
luajava.setFloatingWindowHide(true)

end
function getseekgra()
jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(10)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors({
	0x6600c6ff,0x660072ff
})
jianbians:setStroke(2,"0x44ffffff")--边框宽度和颜色

return jianbians
end
corbk = true
当前ui = 1
function 切换(x)
当前ui = x
luajava.runUiThread(function()
	for i = 1,#stab do
	
	_ENV["layout"..i]:setVisibility(View.GONE)
	end
	_ENV["layout"..当前ui]:setVisibility(View.VISIBLE)
	
	YoYoImpl:with("FadeInLeft"):duration(500):playOn(_ENV["layout"..当前ui])
	end)
end
显示 = 0
显2=false
function 隐藏2()
显2=true
ckou:setVisibility(View.GONE)
floatWindow:setBackground(beij2)
xfc:setVisibility(View.GONE)
smallc:setVisibility(View.VISIBLE)
end
function 显示2()
显2=false
mainLayoutParams.x=20
window:updateViewLayout(floatWindow, mainLayoutParams)

if 显示==1 then
	ckou:setVisibility(View.VISIBLE)
	floatWindow:setBackground(beij)
	smallc:setVisibility(View.GONE)
else
	xfc:setVisibility(View.VISIBLE)
	smallc:setVisibility(View.GONE)
	隐藏()
	
end
end
beij = getVerticalBG({0xffd8dddb,0xffd8dddb},20,3,0xff04d768)
beij2 = luajava.loadlayout({
	GradientDrawable,
	color = "#001E1C27",
	cornerRadius = 10
})
function getcolor(cl)
cl[1] = tonumber(math.ceil(cl[1]*2.6,0,5))

if cl[1] > 255 then cl[1] = "0xff" else
	cl[1] = "0x"..string.format("%x",cl[1]) end
for i = 1,3 do
cl[i+1] = string.format("%x",cl[i+1])
if string.len(cl[i+1]) == 1 then cl[i+1] = "0"..cl[i+1] end
end
cl = cl[1]..cl[2]..cl[3]..cl[4]
return cl
end
function getrgb(cl)
if string.sub(cl,1,1) == "#" then cl = "0x"..string.sub(cl,2,-1) end
cl = {
	tonumber(string.sub(cl,0,4)),tonumber("0x"..string.sub(cl,5,6)),tonumber("0x"..string.sub(cl,7,8))}
return cl
end
function 隐藏()
if 显2==true then return 0 end
luajava.runUiThread(function()
	if tonumber(tostring(ckou:getVisibility())) == 8.0 then
--chuangk:setVisibility(View.VISIBLE)
	ckou:setVisibility(View.VISIBLE)
	xfc:setVisibility(View.GONE)
	显示=1
	mainLayoutParams.flags = LayoutParams.FLAG_NOT_TOUCH_MODAL
	window:updateViewLayout(floatWindow, mainLayoutParams)
	_ENV["layout"..当前ui]:setVisibility(View.VISIBLE)
	changan.controlBig(floatWindow,800)
	floatWindow:setBackground(beij)
	else
		显示=0
	luajava.startThread(function()
	luajava.runUiThread(function()
		changan.controlSmall(floatWindow,500)
	end)
	gg.sleep(500)
	luajava.runUiThread(function()
	mainLayoutParams.flags = LayoutParams.FLAG_NOT_FOCUSABLE
	window:updateViewLayout(floatWindow, mainLayoutParams)
	
	floatWindow:setBackground(beij2)
	ckou:setVisibility(View.GONE)
	xfc:setVisibility(View.VISIBLE)
	_ENV["layout"..当前ui]:setVisibility(View.GONE)
	changan.controlBig(floatWindow,500)
	end)
	end)
	end
	end)
end
function guid()
local seed = {
	'e','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
}
tb = {}
for i = 1,32 do
table.insert(tb,seed[math.random(1,16)])
end
sid = table.concat(tb)
return string.format('%s%s%s%s%s',
	string.sub(sid,1,8),
	string.sub(sid,10,12),
	string.sub(sid,21,22))
..string.format('%s%s%s%s%s',
	string.sub(sid,1,6),
	string.sub(sid,21,25)
)
end

chazhi = {} chajv = {}
function changan.seek(name,bian,smin,smax,nows)
_ENV[bian] = nows
local thum= getVerticalBG({0xff04d768,0xff04d768},8,3,0xff6be0c6)
thum:setSize(20, 50)
smin = tonumber(smin) smax = tonumber(smax)
chajv[bian] = smax-smin
chazhi[bian] = 1-smin
if smin == nil then smin = 1 smax = 10 end
truesmin = 1
truesmax = truesmin+chajv[bian]
if not nows then nows = smin tnows = (smin-nows)
else
	tnows = (nows-smin)+1
end
if _ENV[bian] == nil then _ENV[bian] = 1.0 end
if not name then name = "未设置" end
local names = name..guid()
rest = luajava.loadlayout({
	LinearLayout,
	layout_width = 'match_parent',
	gravity='center_vertical',
	{
		FrameLayout,
		layout_width = 'match_parent',
		layout_hight = "fill_parent",
		layout_weight=1,
		layout_marginTop = "5dp",
		layout_marginBottom = "5dp",
		gravity = "center_vertical",
		background=getVerticalBG({0xE10000FF,0xE10000FF},3,3,0xff6be0c6),
		{
			SeekBar,
			layout_width = 'match_parent',
			min = truesmin,
			max = truesmax,
			progress = tnows,
			thumb=thum,
			progressDrawable={getVerticalBG({0x00ffffff,0x00ffffff},3,3,0x006be0c6)},
			onSeekBarChange = {
				onProgressChanged = function(SeekBar, var2, var3)
				if not var3 then
				return
				end
				local resultvar = tonumber(string.sub(var2,0,-3))-chazhi[bian]
				luajava.runUiThread(function()
					luajava.getIdValue(names):setText(tostring(resultvar))
					end)
				_ENV[bian] = resultvar
				end
			}},{
			TextView,
			layout_gravity = "center",
			text = tostring(nows),
			id = luajava.newId(names),
			gravity='center',
			textSize='11sp',
			textColor='#000000',
		}
	},
	{TextView,
		layout_width='60dp',
		layout_marginLeft='8dp',
		text=name,
		textSize='11sp',
		textColor='#000000',
	}
})

return rest
end
radon=getRes("heiraon")
radoff=getRes("heiraoff")
radiog={}
function changan.radio (cklist)
local rid=guid()
radiog[rid]={}
rest = {
	LinearLayout ,
	layout_width = 'match_parent' ,
	layout_height = "wrap_content" ,
	layout_marginTop = "10dp" ,
	gravity = "top" ,
	orientation = "vertical" ,

}
if type (cklist [1]) == "string" then
	rds=2
rest [# rest + 1] = {
	TextView ,
	gravity = "left" ,
	padding="5dp",
	text = cklist [1] ,
	textSize = "13sp" ,
	textColor = '#000000' ,
	layout_width = 'fill_parent' ,
	layout_height = 'wrap_content' ,
	layout_marginLeft = "10dp" ,
	layout_marginRight = "5dp" ,
	layout_marginTop = "0dp" ,
	layout_marginBottom = "0dp" ,
}
else
	rds=1
end
local restt={
	LinearLayout ,
	layout_width = 'match_parent' ,
	layout_height = "wrap_content" ,
	layout_marginTop = "10dp" ,
	gravity = "top" ,
	
}
for i = rds , # cklist do
local name = cklist [i] [1]
local func = cklist [i] [2]
if not name then
name = "未设置"
end
nid = name..guid ()
radiog[rid][nid]=false
local func = radin(rid,nid,func)
local tid = nid..guid ()
_ENV [tid] = luajava.loadlayout ( {
	LinearLayout ,
	layout_height = "30dp" ,
	layout_marginTop = "5dp" ,
	layout_marginBottom = "15dp" ,
	layout_marginLeft = "4dp" ,
	layout_marginRight = "10dp" ,
	gravity = "center_vertical" ,
	onClick = function ()
	changan.controlWater (_ENV [tid] , 200)
	func()
	end

	,
	{
		ImageView ,
		id = luajava.newId (nid) ,
		layout_width = '20dp' ,
		layout_height = "20dp" ,
		layout_marginLeft = "2dp" ,
		layout_marginRight = "0dp" ,
		src =radoff ,
		colorFilter=0xff04d768,
	} , {
		TextView ,
		gravity = "top" ,
		text = name ,
		textSize="11sp",
		textColor = '#000000' ,
		layout_width = 'wrap_content' ,
		layout_height = 'wrap_content' ,
		layout_marginLeft = "1dp" ,
		layout_marginRight = "3dp" ,
	}
})
restt [# restt + 1] = _ENV [tid]
end
rest [# rest + 1] = restt
return luajava.loadlayout (rest)
end
function radin(rid,nid,func)
return function()
for k,v in pairs(radiog[rid]) do
	luajava.getIdValue(k):setImageDrawable(radoff)
	if k==nid and v~=true then
		luajava.getIdValue(k):setImageDrawable(radon)
		v=true
		luajava.newThread(func):start()
	end
end
end
end


function 开关3(name,func1,func2,nid)
name = name..guid()
_ENV[name] = "关"
if func1 == nil then func1 = "" end
if func2 == nil then func2 = "" end
if type(func1) == "function" then
return function()
namers = _ENV[name]
if namers ~= "开" then
luajava.runUiThread(function()
	luajava.getIdValue(nid.."k"):setVisibility(View.GONE)
	luajava.getIdValue(nid.."g"):setVisibility(View.VISIBLE)
	end)
_ENV[name] = "开"
vibra:vibrate(10)
pcall(func1)
else
	luajava.runUiThread(function()
	luajava.getIdValue(nid.."g"):setVisibility(View.GONE)
	luajava.getIdValue(nid.."k"):setVisibility(View.VISIBLE)
	end)
_ENV[name] = "关"
vibra:vibrate(10)

pcall(func2)
end
end
end
end
function getShape3()
jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(12)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors({0x00000000,0x00000000})
jianbians:setOrientation(GradientDrawable.Orientation.LEFT_RIGHT)
jianbians:setStroke(2,0xaaffffff)--边框宽度和颜色
return jianbians
end
function getShape(tmp0,tmp1,tmp2,tmp3)
jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(tmp0)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors(tmp1)
jianbians:setOrientation(GradientDrawable.Orientation.LEFT_RIGHT)
jianbians:setStroke(8,tmp3)--边框宽度和颜色
return jianbians
end
function getShape2(tmp0,tmp1,tmp2,tmp3)
jianbians = luajava.new(GradientDrawable)
jianbians:setCornerRadius(tmp0)
jianbians:setGradientType(GradientDrawable.LINEAR_GRADIENT)
jianbians:setColors(tmp1)
jianbians:setOrientation(GradientDrawable.Orientation.LEFT_RIGHT)
jianbians:setStroke(8,tmp3)--边框宽度和颜色
return jianbians
end



checkbg1 = getRes("bbts_checkoff")
checkbg2 = getRes("bbts_check")
switchs={}
function changan.intcheck(name,func1,func2)
nid = name..guid()
local func = 开关5(name,func1,func2,nid)
if not name then name = "未设置" end
switchs[nid] = {
	LinearLayout,
	layout_width = 'match_parent',
	layout_weight=1,
	layout_height = "28dp",
	layout_marginTop = "1dp",
	layout_marginBottom = "1dp",
	padding = "1dp",
	{
		LinearLayout,
		padding="3dp",
		onClick = function() luajava.newThread(function() func() end):start() end,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "center_vertical",
		--background=getVerticalBG({0xffFFFDF2,0xE10000FF,0xffFFFDF2},15,8,0xffFFDA71),
		{
			ImageView,
			id = luajava.newId(nid),
			src = checkbg1,
			layout_width = '20dp',
			layout_height = '20dp',
			padding = "0dp",
			colorFilter=0xff04d768,
		},{
			TextView,
			id=luajava.newId(nid.."t"),
			gravity = "left",
			text = name,
			textColor='#000000',
			textSize = "11sp",
--layout_marginLeft="8dp",
			layout_width = 'match_parent',
	layout_weight=1,
		},
		}
}
return switchs[nid]
end
function 开关5(name,func1,func2,nid)
local sname = nid
local localname=name
name = name..guid()
_ENV[name] = "关"
if func1 == nil then func1 = "" end
if func2 == nil then func2 = "" end
if type(func1) == "function" then
return function()
namers = _ENV[name]
if namers ~= "开" then
vibra:vibrate(9)
luajava.runUiThread(function()
	luajava.getIdValue(nid):setImageDrawable(checkbg2)
	--luajava.getIdValue(nid.."t"):setTextColor(switch颜色)
	--changan.controlWater(switchs[nid],300)
	end)
_ENV[name] = "开"
pcall(func1)
else
	vibra:vibrate(9)
luajava.runUiThread(function()
	luajava.getIdValue(nid):setImageDrawable(checkbg1)
	--luajava.getIdValue(nid.."t"):setTextColor(0xff232323)
	--changan.controlWater(switchs[nid],300)
	end)
_ENV[name] = "关"
pcall(func2)
end
end
end
end
function changan.check2(cklist)
	if #cklist==0 then return nil end
	local rest = {
		LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "center",
		orientation="vertical",
		
	}

	for i = 1, #cklist,2 do
	local tempTable = {LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "left",
		orientation="horizontal"
	}
	for j = 0, 1 do
		if cklist[i + j] ~= nil then
		local name = cklist[i + j][1]
		local func1 = cklist[i + j][2]
		local func2 = cklist[i + j][3]
		if not name then name = "未设置" end
			rstt = changan.intcheck(name,func1,func2)
			table.insert(tempTable, rstt)
		else
			table.insert(tempTable, {LinearLayout,
			layout_width = 'match_parent',
			layout_weight=1,})
		end
	end
	table.insert(rest, tempTable)
	end
	return luajava.loadlayout(rest)
end
function changan.check3(cklist)
	if #cklist==0 then return nil end
	local rest = {
		LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "center",
		orientation="vertical",
		
	}

	for i = 1, #cklist,3 do
	local tempTable = {LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "left",
		orientation="horizontal"
	}
	for j = 0, 2 do
		if cklist[i + j] ~= nil then
		local name = cklist[i + j][1]
		local func1 = cklist[i + j][2]
		local func2 = cklist[i + j][3]
		if not name then name = "未设置" end
			rstt = changan.intcheck(name,func1,func2)
			table.insert(tempTable, rstt)
		else
			table.insert(tempTable, {LinearLayout,
			layout_width = 'match_parent',
			layout_weight=1,})
		end
	end
	table.insert(rest, tempTable)
	end
	return luajava.loadlayout(rest)
end
function changan.check1(cklist)
	if #cklist==0 then return nil end
	local rest = {
		LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "center",
		orientation="vertical",
		
	}

	for i = 1, #cklist,1 do
	local tempTable = {LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "wrap_content",
		gravity = "left",
		orientation="vertical"
	}
	for j = 0, 0 do
		if cklist[i + j] ~= nil then
		local name = cklist[i + j][1]
		local func1 = cklist[i + j][2]
		local func2 = cklist[i + j][3]
		if not name then name = "未设置" end
			rstt = changan.intcheck(name,func1,func2)
			table.insert(tempTable, rstt)
		else
			table.insert(tempTable, {LinearLayout,
			layout_width = 'match_parent',
			layout_weight=1,})
		end
	end
	table.insert(rest, tempTable)
	end
	return luajava.loadlayout(rest)
end
switches = {}
function 开关3(name,func1,func2,nid)
local sname = nid
local localname=name
name = name..guid()
_ENV[name] = "关"
if func1 == nil then func1 = "" end
if func2 == nil then func2 = "" end
if type(func1) == "function" then
local outfunc=function()
namers = _ENV[name]
if namers ~= "开" then
vibra:vibrate(9)
luajava.runUiThread(function()
	luajava.getIdValue(nid.."k"):setVisibility(View.GONE)
	YoYoImpl:with("ZoomInLeft"):duration(600):playOn(switches["2s"..sname])
	luajava.getIdValue(nid.."g"):setVisibility(View.VISIBLE)
luajava.getIdValue(nid):setBackground(checkbg)
	end)
_ENV[name] = "开"

pcall(func1)
else
	vibra:vibrate(9)
luajava.runUiThread(function()
	luajava.getIdValue(nid.."g"):setVisibility(View.GONE)
	YoYoImpl:with("ZoomInRight"):duration(600):playOn(switches["1s"..sname])
	luajava.getIdValue(nid.."k"):setVisibility(View.VISIBLE)
luajava.getIdValue(nid):setBackground(checkbga)
	end)
_ENV[name] = "关"
pcall(func2)
end
end
if localname=="摇一摇隐藏UI" then yyfunc=outfunc end
if localname=="音量键隐藏UI" then ylfunc=outfunc end
return outfunc
end
end

function changan.switch(name,func1,func2,miaoshu)
if type(func1)~='function' then func1=function() end end
if type(func2)~='function' then func2=function() end end

if not checkbg then
	checkbg = getVerticalBG({0xff04d768,0xff04d768},90)
checkbga = getVerticalBG({0xffE6E6E6,0xffE6E6E6},90)
switchbg1 = getVerticalBG({0xE10000FF,0xE10000FF},90)
switchbg2 = luajava.loadlayout {
			GradientDrawable ,
			color = "#ffffff" ,
			cornerRadius = 360
		}
end
nid = name..guid()
local func = 开关3(name,func1,func2,nid)
if not name then name = "未设置" end
switches["1s"..nid] = luajava.loadlayout {
	FrameLayout,
	layout_width = '40dp',
	layout_height = '20dp',
	gravity = "center_vertical",
	padding = {
		"1dp","0dp","1dp","0dp"
	},
	{
		LinearLayout,
		layout_gravity = "left|center_vertical",
		id = luajava.newId(nid.."k"),
		background = switchbg1,
		onClick = function() luajava.newThread(function() func() end):start() end,
		layout_width = '17dp',
		layout_height = '17dp',
		
	},
}
switches["2s"..nid] = luajava.loadlayout {
	FrameLayout,
	onClick = function() luajava.newThread(function() func() end):start() end,
	layout_width = '40dp',
	layout_height = '20dp',
	gravity = "center_vertical",
	padding = {
		"1dp","0dp","1dp","0dp"
	}
	, {
		LinearLayout,
		visibility = "gone",
		layout_gravity = "right|center_vertical",
		id = luajava.newId(nid.."g"),
		background = switchbg2,
		onClick = function() luajava.newThread(function() func() end):start() end,
		layout_width = '17dp',
		layout_height = '17dp',

		
	}
}
rest = luajava.loadlayout({
	LinearLayout,
	elevation = "5dp",
	layout_width = 'fill_parent',
	layout_height = "48dp",
	gravity = "center_vertical",
	{
		LinearLayout,
		layout_width = 'fill_parent',
		layout_height = "40dp",
		gravity = "center_vertical",
		background = luajava.loadlayout {
			GradientDrawable ,
			color = 按钮颜色 ,
			cornerRadius = 35
		} ,padding = {
		"0dp","0dp","6dp","0dp"
	},
		{
			FrameLayout,
			id=luajava.newId(nid),
			background = checkbga,
			elevation = "1dp",
			onClick = function() luajava.newThread(function() func() end):start() end,
			layout_width = 'wrap_content',
			layout_height = 'wrap_content',
			gravity = "left",
			padding="1dp",
			switches["1s"..nid],switches["2s"..nid]
		},{
			TextView,
			gravity = "top",
			text = name,
			textColor = "#000000",
			textSize = "13sp",
			layout_width = 'wrap_content',
			layout_marginLeft = "10dp",
			layout_marginRight = "20dp",
		}}
})
return rest
end
function changan.edit(name)
_ENV[name] = name..guid()
if not name then name = "点击输入文字" end
local rest = luajava.loadlayout({
	LinearLayout,
	layout_width = 'fill_parent',
	layout_hight = "fill_parent",
	{
		LinearLayout,
		layout_width = 'fill_parent',
		layout_hight = "fill_parent",
		layout_marginTop = "5dp",
		layout_marginBottom = "5dp",
		layout_marginLeft = "10dp",
		layout_marginRight = "10dp",
		gravity = "center_vertical",
		background = getseekgra(),
		{
			EditText,
			gravity = "top",
			hint = name,
			gravity = "center",
			id = luajava.newId(_ENV[name]),
			layout_width = 'fill',
			layout_marginLeft = "10dp",
			layout_marginRight = "10dp",
		}}
})

return rest
end


function getButtonBG()
local selector = luajava.getStateListDrawable()
selector:addState({
	android.R.attr.state_pressed
}, getVerticalBG({0xff6be0c6,0xff6be0c6},10,2,0xff232323))
selector:addState({
	-android.R.attr.state_pressed
}, getVerticalBG({0xff04d768,0xff04d768},10,2,0xff6be0c6))
return selector
end
function changan.button(txt,func)
if not txt then txt = "未设置" end
return luajava.loadlayout(
	{
		LinearLayout,
		layout_width = 'match_parent',
		layout_hight = "30dp", {
			LinearLayout,
			layout_width = "fill_parent",
			gravity = "center_horizontal",
			layout_marginTop = "5dp",
			layout_marginBottom = "5dp",
			background = getButtonBG(),
			onClick = function() luajava.newThread(function() pcall(func) end):start() end,
			{
				TextView,
--id = luajava.newId(tid),
				layout_marginTop = "5dp",
			layout_marginBottom = "5dp",
				text = txt,
				textColor='#000000',
				textSize = "12sp",
				layout_width = "wrap_content",
			},
		}})
end
import("android.media.AudioManager")
audi = context:getSystemService("audio")
audiotype = {
	AudioManager.STREAM_ALARM, --手机闹铃的声音
	AudioManager.STREAM_MUSIC, --手机音乐的声音
	AudioManager.STREAM_NOTIFICATION, --系统提示的通知
	AudioManager.STREAM_RING, --电话铃声的声音
	AudioManager.STREAM_SYSTEM, --手机系统的声音
	AudioManager.STREAM_VOICE_CALL, --语音电话的声音
	AudioManager.STREAM_DTMF, --DTMF音调的声音
--AudioManager.STREAM_BLUETOOTH_SCO,
}
yinl = {}
for i = 1,#audiotype do
yinl[i] = {}
yinl[i].type = audiotype[i]
yinl[i].min = audi:getStreamMinVolume(audiotype[i])
yinl[i].max = audi:getStreamMaxVolume(audiotype[i])
yinl[i].now = audi:getStreamVolume(audiotype[i])
end
yltype = 0
function jianting3(func)
yinln = {}
for i = 1,#audiotype do
yinln[i] = {}
yinln[i].type = audiotype[i]
yinln[i].now = audi:getStreamVolume(audiotype[i])
if yinln[i].now > yinl[i].now then
yinl[i].now = yinln[i].now
if yltype == 1 then
yltype = 0
func()
end
elseif yinln[i].now < yinl[i].now then
yinl[i].now = yinln[i].now
if yltype == 0 then
yltype = 1
func()
end
end
end
end
qhkai = 0
qiehuan = function()
if qhkai == 0 then
qhkai = 1
draw.remove()
luajava.runUiThread(function()
	changan.controlSmall(floatWindow,400)
	end)
gg.sleep(400)
luajava.runUiThread(function()
	floatWindow:setVisibility(View.GONE)
	end)
else
	qhkai = 0
huiz()
luajava.runUiThread(function() floatWindow:setVisibility(View.VISIBLE) end)
luajava.runUiThread(function()
	changan.controlBig(floatWindow,400)
	end)

end
end
function changan.text(txt,color,size)
if not txt then txt = "未设置文字" end
if not color then color = "#000000" end
if not size then size = "18sp" end
return luajava.loadlayout(
	{
		TextView,
		text = txt,
		textSize = size,
		textColor = color,
		layout_width = "wrap_content",
	})
end
corb = true
function changan.setedit(name,txt)
txt = tostring(txt)
luajava.runUiThread(function()
	luajava.getIdValue(_ENV[name]):setText(txt)
	end)
end

function changan.getedit(name)
--gg.alert(edit)
edit = tostring(luajava.getIdValue(_ENV[name]):getText())
return edit
end
function 开关(name,func1,func2)
if func1 == nil then func1 = "" end
if func2 == nil then func2 = "" end
if type(func1) == "function" then
return function()
namers = _ENV[name]
if namers ~= "开" then
_ENV[name] = "开"
pcall(func1)
else
	_ENV[name] = "关"
pcall(func2)
end

end
end
end
paramt = {}
titletable = {}
corb = true
function getLayoutParams2()
local prm = luajava.new(WindowManager.LayoutParams)

layoutParams1 = prm
if (Build.VERSION.SDK_INT >= 26) then -- 设置悬浮窗方式
layoutParams1.type = prm.TYPE_APPLICATION_OVERLAY
else
	layoutParams1.type = prm.TYPE_PHONE
end
layoutParams1.format = PixelFormat.RGBA_8888 -- 设置背景
layoutParams1.flags = prm.FLAG_NOT_FOCUSABLE -- 焦点设置Finish
layoutParams1.gravity = Gravity.CENTER -- 重力设置
layoutParams1.width = prm.WRAP_CONTENT -- 布局宽度
layoutParams1.height = prm.WRAP_CONTENT -- 布局高度
return layoutParams1
end

namelist = {}
param1 = {}
floattable = {}
function changan.newfloat(name,func1,func2)
floattable[name] = 1
local func = 开关(name.."k",func1,func2)
window = context:getSystemService("window") -- 获取窗口管理器
local function invoke(name,func1,func2)
if not name then name = "未设置" end
nameid = name..guid()
local ok
local RawX, RawY, x, y
nameid1 = name..guid()
if not namelist[name] then namelist[name] = false end
if namelist[name] ~= false then clclcl = "#75ff0000" else clclcl = "#880CFF76" end
param1[name] = getLayoutParams2()
_ENV[name] = luajava.loadlayout(
	{
		LinearLayout,
		layout_width = "40dp",
		id = luajava.newId(nameid1),
		layout_height = "40dp",
		background = luajava.loadlayout {
			GradientDrawable,
			color = clclcl,
			cornerRadius = 30
		},
		onClick = function()

		if namelist[name] == false then
		_ENV[name]:setBackground(luajava.loadlayout {
			GradientDrawable,
			color = "#75ff0000",
			cornerRadius = 30
		})
		namelist[name] = true
		elseif namelist[name] == true then
		_ENV[name]:setBackground(luajava.loadlayout {
			GradientDrawable,
			color = "#880CFF76",
			cornerRadius = 30
		})
		namelist[name] = false
		end
		luajava.newThread(function() pcall(func) end):start()
		end,

		onTouch = function(v, event)
		local Action = event:getAction()
		if Action == MotionEvent.ACTION_DOWN then
		isMove = false
		RawX = event:getRawX()
		RawY = event:getRawY()
		x = param1[name].x
		y = param1[name].y
		elseif Action == MotionEvent.ACTION_MOVE then
		isMove = true
		param1[name].x = tonumber(x) + (event:getRawX() - RawX)
		param1[name].y = tonumber(y) + (event:getRawY() - RawY)
		window:updateViewLayout(_ENV[name], param1[name])
		end
		end,
		{
			TextView,
			text = name,
			gravity = "center",
			layout_width = "50dp",
			layout_height = "50dp",
		}
	})

local function invoke2()
window:addView(_ENV[name], param1[name])
end

local runnable = luajava.getRunnable(invoke2)
local handler = luajava.getHandler()
handler:post(runnable)

end

invoke(name,func1,func2)
end

function changan.rmvfloat(name)
floattable[name] = 0
local function invoke2()
window:removeView(_ENV[name], param1[name])
end
local runnable = luajava.getRunnable(invoke2)
local handler = luajava.getHandler()
handler:post(runnable)

end

function 开关2(name,func1,func2,nid)
if func1 == nil then func1 = "" end
if func2 == nil then func2 = "" end
if type(func1) == "function" then
return function()
namers = _ENV[name]
if namers ~= "开" then
luajava.runUiThread(function()
	luajava.getIdValue(nid):setBackground(luajava.getBitmapDrawable("/sdcard/小研/图片/check2"))
	end)
_ENV[name] = "开"
func1()
else
	luajava.runUiThread(function()
	luajava.getIdValue(nid):setBackground(luajava.getBitmapDrawable("/sdcard/小研/图片/check1"))
	end)
_ENV[name] = "关"
func2()
end

end
end
end
function visi(tid,ttid)
local tview = luajava.getIdValue(tid)
local ttview = luajava.getIdValue(ttid)
if not tview then return 0 end
if tonumber(tostring(tview:getVisibility())) == 8.0 then
tview:setVisibility(View.VISIBLE)
ttview:setBackground(luajava.getBitmapDrawable("/sdcard/小研/图片/sanjiao"))
else
	tview:setVisibility(View.GONE)
ttview:setBackground(luajava.getBitmapDrawable("/sdcard/小研/图片/hsanjiao"))
end
end
changan.controlBig = function(control,time)
luajava.runUiThread(function()
	import "android.animation.ObjectAnimator"
	ObjectAnimator():ofFloat(control,"scaleX", {
		0, 0.4, 0.7, 1
	}):setDuration(time):start()
	ObjectAnimator():ofFloat(control,"scaleY", {
		0, 0.4, 0.7, 1
	}):setDuration(time):start()
	end) end
changan.controlFlip = function(control,time)
luajava.runUiThread(function()
	import "android.view.animation.Animation"
	import "android.animation.ObjectAnimator"
	xuanzhuandonghua = ObjectAnimator:ofFloat(control, "rotationY", {
		0, 360
	})
	xuanzhuandonghua:setRepeatCount(0)
	xuanzhuandonghua:setRepeatMode(Animation.REVERSE)
	xuanzhuandonghua:setDuration(time)
	xuanzhuandonghua:start()
	end) end
changan.controlWater = function(control,time)
luajava.runUiThread(function()
	import "android.animation.ObjectAnimator"
	ObjectAnimator():ofFloat(control,"scaleX", {
		1, 0.8, 0.9, 1
	}):setDuration(time):start()
	ObjectAnimator():ofFloat(control,"scaleY", {
		1,0.8,0.9,1
	}):setDuration(time):start()
	end) end
changan.controlSmall = function(control,time)
luajava.runUiThread(function()
	import "android.animation.ObjectAnimator"
	ObjectAnimator():ofFloat(control,"scaleX", {
		1, 0.7, 0.4, 0
	}):setDuration(time):start()
	ObjectAnimator():ofFloat(control,"scaleY", {
		1, 0.7, 0.4, 0
	}):setDuration(time):start()
end) end

function changan.box(views)
local tid = "box"..guid()
local ttid = tid.."6"
local firadio = {
	LinearLayout,
	layout_width = 'fill_parent',
	layout_height = "wrap_content",
	layout_marginTop = "2dp",
	gravity="center",
	layout_marginBottom = "2dp",
	orientation = "vertical",
}
if type(views[1]) == "string" or type(views[1]) == "number" then
firadio[#firadio+1] = {
	FrameLayout,
	layout_width = 'match_parent',
	layout_height = "30dp",
	gravity = "center_vertical",
	layout_marginTop = "2dp",
	layout_marginBottom = "4dp",
	onClick = function() visi(tid,ttid) end,
	background = getButtonBG(),
	{
		ImageView,
		layout_gravity="left|center",
		layout_marginLeft = "10dp",
		id = luajava.newId(ttid),
		background = "/sdcard/小研/图片/hsanjiao",
		layout_width = "12dp",
		layout_height = "12dp",
		layout_marginTop = "0dp",
	},
	{
		TextView,text = views[1],
		textSize = "11sp",
		layout_width = "220dp",
		textColor = "#000000",
		layout_gravity = "center",
		layout_marginLeft='34dp',
		
	}} else
	gg.alert("changan.box第一个参数必须是string") os.exit()
end
radios = {
	LinearLayout,
	layout_marginLeft = "0dp",
	layout_marginRight = "0dp",
	orientation = "vertical",
	visibility = "gone",
	id = luajava.newId(tid),
	padding = "0dp",
	gravity="center_horizontal",
	layout_width = 'fill_parent',
}
for i = 2,#views do
radios[#radios+1] = views[i]
end
firadio[#firadio+1] = radios
return luajava.loadlayout(firadio)
end
fenye={}
fenyed={}
function 二级分页(tab)
	if tab['第3页']~=nil then
	local tid=guid()
	fenye[tid]={}
	fenyed[tid]=getVerticalBG({0xE10000FF,0xE10000FF},15)
	for i=1,3 do
		fenye[tid][i]={
		LinearLayout,
		visibility='gone',
		layout_width='match_parent',
		orientation='vertical',
		
		}
		for j=1,#tab['第'..i..'页'] do
			table.insert(fenye[tid][i],tab['第'..i..'页'][j])
		end
		fenye[tid][i]=luajava.loadlayout(fenye[tid][i])
	end
	local tmp={
		LinearLayout,
		layout_width='match_parent',
		gravity='center_horizontal',
		orientation='vertical',
		{LinearLayout,
			padding='2dp',
			background=getVerticalBG({0xff04d768,0xff04d768},15),
			{TextView,
				text=tab['第1页名字'],
				textSize='11sp',
				textColor='#000000',
				onClick=function()
					luajava.getIdValue(tid..'1'):setBackground(fenyed[tid])
					fenye[tid][1]:setVisibility(View.VISIBLE)
					YoYoImpl:with("SlideInUp"):duration(500):playOn(fenye[tid][1])
					luajava.getIdValue(tid..'2'):setBackground(empty)
					fenye[tid][2]:setVisibility(View.GONE)
					luajava.getIdValue(tid..'3'):setBackground(empty)
					fenye[tid][3]:setVisibility(View.GONE)
				end,
				id=luajava.newId(tid..'1'),
				padding={'10dp','5dp','10dp','5dp'},
			},
			{TextView,
				text=tab['第2页名字'],
				textSize='11sp',
				textColor='#000000',
				onClick=function()
					luajava.getIdValue(tid..'1'):setBackground(empty)
					fenye[tid][1]:setVisibility(View.GONE)
					luajava.getIdValue(tid..'2'):setBackground(fenyed[tid])
					YoYoImpl:with("SlideInUp"):duration(500):playOn(fenye[tid][2])
					fenye[tid][2]:setVisibility(View.VISIBLE)
					luajava.getIdValue(tid..'3'):setBackground(empty)
					fenye[tid][3]:setVisibility(View.GONE)
					
				end,
				id=luajava.newId(tid..'2'),
				padding={'10dp','5dp','10dp','5dp'},
			},{TextView,
				text=tab['第3页名字'],
				textSize='11sp',
				textColor='#000000',
				onClick=function()
					luajava.getIdValue(tid..'1'):setBackground(empty)
					fenye[tid][1]:setVisibility(View.GONE)
					luajava.getIdValue(tid..'2'):setBackground(empty)
					YoYoImpl:with("SlideInUp"):duration(500):playOn(fenye[tid][3])
					fenye[tid][2]:setVisibility(View.GONE)
					luajava.getIdValue(tid..'3'):setBackground(fenyed[tid])
					fenye[tid][3]:setVisibility(View.VISIBLE)
					
				end,
				id=luajava.newId(tid..'3'),
				padding={'10dp','5dp','10dp','5dp'},
			}
		},fenye[tid][1],fenye[tid][2],fenye[tid][3]
	}
	local tmp= luajava.loadlayout(tmp)
	luajava.getIdValue(tid..'1'):setBackground(fenyed[tid])
	fenye[tid][1]:setVisibility(View.VISIBLE)
	return tmp
	else
	local tid=guid()
	fenye[tid]={}
	fenyed[tid]=getVerticalBG({0xE10000FF,0xE10000FF},15)
	for i=1,2 do
		fenye[tid][i]={
		LinearLayout,
		visibility='gone',
		layout_width='match_parent',
		orientation='vertical',
		
		}
		for j=1,#tab['第'..i..'页'] do
			table.insert(fenye[tid][i],tab['第'..i..'页'][j])
		end
		fenye[tid][i]=luajava.loadlayout(fenye[tid][i])
	end
	local tmp={
		LinearLayout,
		layout_width='match_parent',
		gravity='center_horizontal',
		orientation='vertical',
		{LinearLayout,
			padding='2dp',
			background=getVerticalBG({0xff04d768,0xff04d768},15),
			{TextView,
				text=tab['第1页名字'],
				textSize='11sp',
				textColor='#000000',
				onClick=function()
					luajava.getIdValue(tid..'1'):setBackground(fenyed[tid])
					fenye[tid][1]:setVisibility(View.VISIBLE)
					YoYoImpl:with("SlideInUp"):duration(500):playOn(fenye[tid][1])
					luajava.getIdValue(tid..'2'):setBackground(empty)
					fenye[tid][2]:setVisibility(View.GONE)
				end,
				id=luajava.newId(tid..'1'),
				padding={'10dp','5dp','10dp','5dp'},
			},
			{TextView,
				text=tab['第2页名字'],
				textSize='11sp',
				textColor='#000000',
				onClick=function()
					luajava.getIdValue(tid..'1'):setBackground(empty)
					fenye[tid][1]:setVisibility(View.GONE)
					luajava.getIdValue(tid..'2'):setBackground(fenyed[tid])
					YoYoImpl:with("SlideInUp"):duration(500):playOn(fenye[tid][2])
					fenye[tid][2]:setVisibility(View.VISIBLE)
					
				end,
				id=luajava.newId(tid..'2'),
				padding={'10dp','5dp','10dp','5dp'},
			}
		},fenye[tid][1],fenye[tid][2]
	}
	local tmp= luajava.loadlayout(tmp)
	luajava.getIdValue(tid..'1'):setBackground(fenyed[tid])
	fenye[tid][1]:setVisibility(View.VISIBLE)
	return tmp
end
end






function huiz()
draw.setColor("#FF00FFFF")
draw.setStyle("填充")
draw.setSize(47)
	
	end


stab = {
--菜单名字，添加即可加页数，需要与结尾配置表页数对应
	"防封",
	"功能",
	"美化",
	"音乐",
}

左上角图标='https://pan.jl8.top/view.php/51724829d34a0d071b43afd4eb4bf1ee.jpg'

小悬浮窗图标 = "https://pan.jl8.top/view.php/51724829d34a0d071b43afd4eb4bf1ee.jpg"
--悬浮窗链接或

changan.menu(
	{
		{--1
changan.text('小研全防','#FF000000','15sp'),
changan.text('','#DB202C','10sp'),
changan.text('','#FF000000','10sp'),
				
			
			
	changan.button("公告（必看）",	function()
string.toMusic("小研独家防封独家防禁网都是自己现抓的没有二改那一出只是偷个泛滥UI功能全自己的祝大家天天开心，玩的愉快")
gg.alert("小研独家防封独家防禁网都是自己现抓的没有二改那一出只是偷个泛滥UI功能全自己的祝大家天天开心玩的愉快")
		end),

changan.button("每日语录",function()
			Y=gg.makeRequest("https://v1.hitokoto.cn/").content
--获取云端数据
Q=string.match(Y,'hitokoto(.+)type')
--模式匹配他们中的一切字符
F=string.gsub(Q,'":"',"")
K=string.gsub(F,'","',"")
--删除多余垃圾
string.toMusic("\n\n"..""..K.."")
QD = gg.alert("今日语录:\n\n"..""..K.."")
if QD == 1 then

end
if QD == 3 then 
end
end),

changan.switch("自动选择进程",
			function()
			gg.setProcess("com.tencent.tmgp.pubgmhd")	
			string.toMusic('小研自动选择进程开启成功')
			end,
			function()
				
			end),
			changan.switch("腾讯防封",
				function()
			if gg.getRangesList("libtersafe.so")[1] then
 local t = {}
 t[1] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677030; -- 数值地址:0x6F49D6B030
 t[2] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677190; -- 数值地址:0x6F49D6B190
 t[3] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677558; -- 数值地址:0x6F49D6B558
 t[4] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677E18; -- 数值地址:0x6F49D6BE18
 t[5] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677FB0; -- 数值地址:0x6F49D6BFB0
 t[6] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678398; -- 数值地址:0x6F49D6C398
 t[7] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678778; -- 数值地址:0x6F49D6C778
 t[8] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678A70; -- 数值地址:0x6F49D6CA70
 t[9] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678F80; -- 数值地址:0x6F49D6CF80
 gg.setValues({
  [1] = { 
   address = t[1],
   flags = 4,
   value = 256,
  },
  [2] = { 
   address = t[2],
   flags = 4,
   value = 256,
  },
  [3] = { 
   address = t[3],
   flags = 4,
   value = 256,
  },
  [4] = { 
   address = t[4],
   flags = 4,
   value = 256,
  },
  [5] = { 
   address = t[5],
   flags = 4,
   value = 256,
  },
  [6] = { 
   address = t[6],
   flags = 4,
   value = 256,
  },
  [7] = { 
   address = t[7],
   flags = 4,
   value = 256,
  },
  [8] = { 
   address = t[8],
   flags = 4,
   value = 256,
  },
  [9] = { 
   address = t[9],
   flags = 4,
   value = 256,
  },
 })
 gg.toast("SM的腾讯防封开启成功 ")
end
if gg.getRangesList("libUE4.so")[1] then
 local t = {}
 t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x4; -- 数值地址:0x7046807004
 gg.setValues({
  [1] = { 
   address = t[1],
   flags = 4,
   value = 1024,
  },
 })
 gg.toast("腾讯防封开启成功")
end
if gg.getRangesList("libtprt.so:bss")[1] then
 local t = {}
 t[1] = gg.getRangesList("libtprt.so:bss")[1]["start"] + 0x3294; -- 数值地址:0x70991AD294
 gg.setValues({
  [1] = { 
   address = t[1],
   flags = 4,
   value = 1,
  },
 })
	gg.toast("开启成功")
end
if gg.getRangesList("libtersafe.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677030; -- 数值地址:0x7D23488030
	t[2] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677190; -- 数值地址:0x7D23488190
	t[3] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677558; -- 数值地址:0x7D23488558
	t[4] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677E18; -- 数值地址:0x7D23488E18
	t[5] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x677FB0; -- 数值地址:0x7D23488FB0
	t[6] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678398; -- 数值地址:0x7D23489398
	t[7] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678778; -- 数值地址:0x7D23489778
	t[8] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678A70; -- 数值地址:0x7D23489A70
	t[9] = gg.getRangesList("libtersafe.so")[1]["start"] + 0x678F80; -- 数值地址:0x7D23489F80
	gg.setValues({
		[1] = { 
			address = t[1],
			flags = 4,
			value = 256,
		},
		[2] = { 
			address = t[2],
			flags = 4,
			value = 256,
		},
		[3] = { 
			address = t[3],
			flags = 4,
			value = 256,
		},
		[4] = { 
			address = t[4],
			flags = 4,
			value = 256,
		},
		[5] = { 
			address = t[5],
			flags = 4,
			value = 256,
		},
		[6] = { 
			address = t[6],
			flags = 4,
			value = 256,
		},
		[7] = { 
			address = t[7],
			flags = 4,
			value = 256,
		},
		[8] = { 
			address = t[8],
			flags = 4,
			value = 256,
		},
		[9] = { 
			address = t[9],
			flags = 4,
			value = 256,
		},
	})
	gg.toast("腾讯防开启成功")
end	
end),
                
changan.switch("腾讯杀67logo界面开大厅关",		
function()local Ranges=gg.getRangesList('/')
local function Read(module,type)
    for k,v in pairs(Ranges) do
        if v['internalName']:match('[^/]*$')==module and v['type']==type then
            return v['start']
        end
    end
end

local Table={}
local function Modify(address,value,flags)
    Table[#Table+1]={address=address,value=value,flags=flags}
end

Modify(Read('libUE4.so','r-xp')+0x8,10086,4)
gg.setValues(Table)
end,
function()
local Ranges=gg.getRangesList('/')
local function Read(module,type)
    for k,v in pairs(Ranges) do
        if v['internalName']:match('[^/]*$')==module and v['type']==type then
            return v['start']
        end
    end
end

local Table={}
local function Modify(address,value,flags)
    Table[#Table+1]={address=address,value=value,flags=flags}
end

Modify(Read('libUE4.so','r-xp')+0x8,0,4)
gg.setValues(Table)
end),	


			changan.switch("防闪",
            function()
            local t = {"libtersafe.so:bss", "Cb"}
local tt = {0x3F8}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 4096, freeze = true}})
string.toMusic('开启成功')
		end,
			function()
				
			end,"切屏开"),

changan.switch("登陆防禁网",
function()


Modify(Read('libUE4.so','r-xp')+0x2179aac,256,16)
Modify(Read('libUE4.so','r-xp')+0x244aa40,256,16)
Modify(Read('libUE4.so','r-xp')+0x25c6d1c,256,16)
Modify(Read('libUE4.so','r-xp')+0x25c6d24,256,16)
Modify(Read('libUE4.so','r-xp')+0x25ca394,256,16)
Modify(Read('libUE4.so','r-xp')+0x25ca39c,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cad88,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cad90,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cbce4,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cbd5c,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cc0e8,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cc168,256,16)
Modify(Read('libUE4.so','r-xp')+0x25cc170,256,16)
Modify(Read('libUE4.so','r-xp')+0x25ccf94,256,16)
Modify(Read('libUE4.so','r-xp')+0x25ccfb8,256,16)
Modify(Read('libUE4.so','r-xp')+0x25d4bfc,256,16)
Modify(Read('libUE4.so','r-xp')+0x25d4c04,256,16)
Modify(Read('libUE4.so','r-xp')+0x25d8674,256,16)
Modify(Read('libUE4.so','r-xp')+0x25d867c,256,16)
Modify(Read('libUE4.so','r-xp')+0x25e0bd0,256,16)
Modify(Read('libUE4.so','r-xp')+0x25e0bd8,256,16)
Modify(Read('libUE4.so','r-xp')+0x25e6744,256,16)
Modify(Read('libUE4.so','r-xp')+0x25e674c,256,16)
Modify(Read('libUE4.so','r-xp')+0x25ee118,256,16)
Modify(Read('libUE4.so','r-xp')+0x25ee120,256,16)
Modify(Read('libUE4.so','r-xp')+0x25f6fc4,256,16)
Modify(Read('libUE4.so','r-xp')+0x25f6fcc,256,16)
Modify(Read('libUE4.so','r-xp')+0x25fbc6c,256,16)
Modify(Read('libUE4.so','r-xp')+0x25fbc74,256,16)
Modify(Read('libUE4.so','r-xp')+0x25fc5b8,256,16)
Modify(Read('libUE4.so','r-xp')+0x25fc5c0,256,16)
Modify(Read('libUE4.so','r-xp')+0x25fcb58,256,16)
Modify(Read('libUE4.so','r-xp')+0x25fcb60,256,16)




gg.toast("独家防禁网开启成功")
end),
changan.switch("局内防踢（大厅开）",
function()
local t = {"libtprt.so:bss", "Cb"}
local tt = {0x1F50}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x1F78}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x1F8C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x1FF0}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x26D8}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x2728}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x28A4}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x28B4}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x28FC}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x2928}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x29AC}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x2A48}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x2AF0}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libtprt.so:bss", "Cb"}
local tt = {0x2E60}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})
end),
changan.switch("局内防举报",
function()
local Ranges=gg.getRangesList('/')
local function Read(module,type)
    for k,v in pairs(Ranges) do
        if v['internalName']:match('[^/]*$')==module and v['type']==type then
            return v['start']
        end
    end
end

local Table={}
local function Modify(address,value,flags)
    Table[#Table+1]={address=address,value=value,flags=flags}
end

Modify(Read('libUE4.so','r--p')+0x2a61930,512,4)
Modify(Read('libUE4.so','r--p')+0x2a61934,512,4)
Modify(Read('libUE4.so','r--p')+0x2a61944,512,4)
Modify(Read('libUE4.so','r--p')+0x2a61964,512,4)
Modify(Read('libUE4.so','r--p')+0x2a61974,512,4)
Modify(Read('libUE4.so','r--p')+0x2a6198c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a619a4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a619ac,512,4)
Modify(Read('libUE4.so','r--p')+0x2a619b4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a619c4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62964,512,4)
Modify(Read('libUE4.so','r--p')+0x2a6296c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62970,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62974,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62984,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62988,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62998,512,4)
Modify(Read('libUE4.so','r--p')+0x2a6299c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629a0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629ac,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629b0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629c0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629c4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629d4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629d8,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629e8,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629ec,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629f0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629f4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a629fc,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a00,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a14,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a18,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a1c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a2c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a30,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a40,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a44,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a48,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a58,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a5c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a60,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a70,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a74,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a78,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a7c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a8c,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a90,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62a94,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62aa4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62aa8,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62aac,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ab0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ac0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ac4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ad4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ad8,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62adc,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ae0,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ae4,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62ae8,512,4)
Modify(Read('libUE4.so','r--p')+0x2a62aec,512,4)
Modify(Read('libUE4.so','r--p')+0x342f990,512,4)
Modify(Read('libUE4.so','r--p')+0x3464f74,512,4)
Modify(Read('libUE4.so','r--p')+0x35029e0,512,4)
Modify(Read('libUE4.so','r--p')+0x353b854,512,4)
Modify(Read('libUE4.so','r--p')+0x3575104,512,4)
Modify(Read('libUE4.so','r--p')+0x35a612c,512,4)
Modify(Read('libUE4.so','r--p')+0x35c5200,512,4)
Modify(Read('libUE4.so','r--p')+0x35daf14,512,4)
Modify(Read('libUE4.so','r--p')+0x35ef06c,512,4)
Modify(Read('libUE4.so','r--p')+0x360091c,512,4)
Modify(Read('libUE4.so','r--p')+0x3616dec,512,4)
Modify(Read('libUE4.so','r--p')+0x365b8f0,512,4)
Modify(Read('libUE4.so','r--p')+0x368e46c,512,4)
Modify(Read('libUE4.so','r--p')+0x3699018,512,4)
Modify(Read('libUE4.so','r--p')+0x369abf4,512,4)
Modify(Read('libUE4.so','r--p')+0x369ac00,512,4)
Modify(Read('libUE4.so','r--p')+0x369ac04,512,4)
Modify(Read('libUE4.so','r--p')+0x369ac08,512,4)
Modify(Read('libUE4.so','r--p')+0x369ac50,512,4)
Modify(Read('libUE4.so','r--p')+0x369ac54,512,4)
Modify(Read('libUE4.so','r--p')+0x36cb014,512,4)
Modify(Read('libUE4.so','r--p')+0x36cb020,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc0cc,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc0d0,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc0d4,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc0e4,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc0f0,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc0f4,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc104,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc114,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc118,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc174,512,4)
Modify(Read('libUE4.so','r--p')+0x36cc180,512,4)
Modify(Read('libUE4.so','r--p')+0x36ce6a8,512,4)
Modify(Read('libUE4.so','r--p')+0x3739004,512,4)
Modify(Read('libUE4.so','r--p')+0x43ed6ec,512,4)
Modify(Read('libUE4.so','r--p')+0x46469e4,512,4)
Modify(Read('libUE4.so','r--p')+0x481915c,512,4)
Modify(Read('libUE4.so','r--p')+0x4aeba04,512,4)
Modify(Read('libUE4.so','r--p')+0x4aeba54,512,4)
Modify(Read('libUE4.so','r--p')+0x4b1ff8c,512,4)
Modify(Read('libUE4.so','r--p')+0x5126f8,512,4)
Modify(Read('libUE4.so','r--p')+0x6cdbc8,512,4)
Modify(Read('libUE4.so','r--p')+0x6db6f8,512,4)
Modify(Read('libUE4.so:bss','rw-p')+0x6b808,512,4)
Modify(Read('libINTLCompliance.so','r--p')+0x54,512,4)
Modify(Read('libINTLCompliance.so','r--p')+0x9c,512,4)
gg.setValues(Table)
gg.toast("独家防部分举报  防十年开启成功")
end),
changan.switch("登陆防",		
function()
local t = {"libUE4.so", "Cd"}
local tt = {0x860418, 0xB0, 0x97}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})
	end),	
	changan.switch("登陆防十年",		
function()
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382004,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382014,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38201c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38207c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38209c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382154,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382160,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x3821d4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382230,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382250,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382280,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x3822a0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x3822d4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x382bfc,256,16)
	end),	

							
			changan.switch("登陆防观察期",		
function()
local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xB80}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xB70}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xB90}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xF50}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xB8C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xB04}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0xBAC}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0x878, 0x101C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0x951}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0x8C9}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0x971}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0xDE1}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0x945}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0x935}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0x955}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x500, 0xE98, 0xD15}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xB80}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xB70}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xB90}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xF50}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xB8C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xB04}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})

local t = {"libgsdk.so", "Cd"}
local tt = {0x268, 0x640, 0x7F8, 0xBAC}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 256, freeze = true}})
	end),	
										changan.switch(
			"大厅防1",
			function()
Modify(Read('boot.oat','r--p')+0x14f50,256,16)
Modify(Read('boot.oat','r--p')+0x14f78,256,16)
Modify(Read('boot.oat','r--p')+0x14ff4,256,16)
Modify(Read('boot.oat','r--p')+0x15030,256,16)
Modify(Read('boot.oat','r--p')+0x15040,256,16)
Modify(Read('boot.oat','r--p')+0x15064,256,16)
Modify(Read('boot.oat','r--p')+0x1508c,256,16)
Modify(Read('boot.oat','r--p')+0x150a8,256,16)
Modify(Read('boot.oat','r--p')+0x150c4,256,16)
Modify(Read('boot.oat','r--p')+0x150d4,256,16)
Modify(Read('boot.oat','r--p')+0x150d8,256,16)
Modify(Read('boot.oat','r--p')+0x150e4,256,16)
Modify(Read('boot.oat','r--p')+0x150f8,256,16)
gg.toast('大厅防封开启成功')
string.toMusic("大厅防封开启成功")
			end,
			function()
				
			end),
			changan.switch(
			"大厅防2",
			function()


Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f228,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f23c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f244,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f248,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f24c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f250,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f254,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f260,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f264,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f26c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f274,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f278,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f27c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f284,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f298,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2a0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2a8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2ac,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2b0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2b8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2c0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2c4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f2dc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f4d8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f4e0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f4e8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f4ec,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f4f0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f4fc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f500,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f504,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f50c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f518,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f51c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f52c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f538,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f53c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f540,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f548,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f54c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f550,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f55c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f564,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f56c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f574,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f578,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5c8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5d0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5d8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5dc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5e0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5f0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f5fc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f604,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f608,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f66c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f674,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f67c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f680,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f694,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6a0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6a8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6b0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6b4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6c0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6cc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6dc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6e4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f6e8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f71c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f724,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f72c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f730,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f740,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f748,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f750,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f75c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f764,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f774,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f77c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f794,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f798,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f79c,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7a0,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7a8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7b4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7cc,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7d4,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7d8,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f7ec,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38f800,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38fa38,256,16)
Modify(Read('libPixUI_PXPlugin.so','r-xp')+0x38ffb8,256,16)

gg.toast('大厅防封开启成功')
string.toMusic("大厅防封开启成功")
			end,
			function()
				
			end),
  changan.switch("防下线追封（下线开启）",
				function()
	gg.setValues({
    {
      address = S_Pointer({
        "libtprt.so:bss",
        "Cb"
      }, {2364}, true),
      flags = 4,
      value = 1
    }
  })
  gg.setValues({
    {
      address = S_Pointer({
        "libtprt.so:bss",
        "Cb"
      }, {2384}, true),
      flags = 4,
      value = 1
    }
  })
  string.toMusic("小研独家防下线追封开启成功")
  gg.toast("开启成功")
  gg.toast("开启成功")	
  end),
  changan.switch("虚拟机大厅防",
				function()	
				gg.addListItems({
    {
      address = S_Pointer({
        "libtersafe.so:bss",
        "Cb"
      }, {1184}, true),
      flags = 4,
      value = 4096,
      freeze = true
    }
  })
  string.toMusic("小研独家虚拟机开启成功")
  gg.toast("开启成功")	
  end),				

changan.switch("全局离线（篮圈开结束关）",
function()
so=gg.getRangesList('libgcloud.so')[1].start
py=0x422004
setvalue(so+py,4,0)
so=gg.getRangesList('libgcloud.so')[1].start
py=0x495354
setvalue(so+py,4,0)
string.toMusic('全局离线开启成功')
end,
function()

so=gg.getRangesList('libgcloud.so')[1].start
py=0x422004
setvalue(so+py,4,-1000536719)
so=gg.getRangesList('libgcloud.so')[1].start
py=0x495354
setvalue(so+py,4,-1000536719)
string.toMusic('全局离线关闭成功')
			    end),
			    changan.switch("大厅全火防（大厅开）", 
                function() 
if gg.getRangesList("libspecialgem.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libspecialgem.so")[1]["start"] + 0x35998; -- 数值地址:0x74895B8998
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 4,
			value = -1,
			freeze = true,
		},
	})
end
string.toMusic('全火防开启成功')
gg.toast('全火防开启成功')
                end,
                function()
string.toMusic('无法关闭')
gg.toast('无法关闭')
                end),
						changan.button("退出程序",function()
				window:removeView(floatWindow)
				luajava.setFloatingWindowHide(false)
				tuichu=1
				end),
	},{--第二页
	changan.button("小研功能区",
	
				function()
				
				end),
			changan.switch(
			"加速开/关",
			function()
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = -34564993024.0,
			freeze = (not (25083~=25083)),
		},
	})
end
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = 8.50279631158571E-21,
			freeze = (not (25083~=25083)),
		},
	})
end
gg.alert("关闭成功")
string.toMusic('关闭成功')			
			end),
			    changan.switch('广角',
						function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0xf70,0x2E4}--广角
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 120 }})
gg.alert("开启成功")
							
						end),
						  changan.switch("摇杆加速开启",	
function()
while true do
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0xE98}
local ttt = S_Pointer(t, tt, true)
local  co=gg.getValues({{address=ttt,flags=16,value=nil}})
if co[1].value== 640 then---开启
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = -34564993024.0,
			freeze = (not (25083~=25083)),
		},
	})
end
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x631CB0,0x30,0x558,0x2568}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 20000}})
local tt = {0x631CB0,0x30,0x558,0x19D8,0x16C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 20000}})
local tt = {0x631CB0,0x30,0x88}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 20000}})
local tt = {0x631CB0,0x30,0x558,0x19D8,0x16C+0x4}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 4, value = 0}})
local tt = {0x631CB0,0x7C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 4, value = 3 }})
local tt = {0x631CB0,0x30,0x88}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 999}})
end
if co[1].value== 344.25 then---关闭
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = 8.50279631158571E-21,
			freeze = (not (25083~=25083)),
		},
	})
end
end
end
end,
function()
end),	
				changan.switch('音量判断加速',--默认开，改名就是默认关
				function()
					音量键=true		
				end,
				function()
					音量键=false	
string.toMusic('关闭成功')
end),
				
				changan.switch('飞天',--默认开，改名就是默认关
				function()
			local t = {"libUE4.so:bss", "Cb"}
local tt = {0x6931D0, 0xF8, 0x48}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 1000}})
string.toMusic("开启成功")
end
      , function()
      local t = {"libUE4.so:bss", "Cb"}
local tt = {0x6931D0, 0xF8, 0x48}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 100}})
string.toMusic("关闭成功")
end),

				changan.switch(
			"范围拾取",
			function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x1A30,0x16C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address=ttt,flags=16,value=999999}})	

local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x2670}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address=ttt,flags=16,value=999999}})	
			
	
	local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x1A30,0x170}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address=ttt,flags=4,value=0}})
			end,
			function()
				
			end),
			      changan.switch("无视距离拾取【灵魂  开/关】", function()
        local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70, 0x30, 0x560, 0xA0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 4, value = 0}})
        string.toMusic("开启失败")
      end
      , function()
       local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70, 0x30, 0x560, 0xA0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 4, value = 1}})
        string.toMusic("关闭失败")
      end
      ),
			
				changan.switch(
			"自动抓墙",
			function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x672330, 0x8, 0x8, 0xF0, 0x1E0}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 16, value = 1000, freeze = true}})
			end,
			function()
				
			end),
			changan.switch(
			"刷刀体质（有点拉回）",
			function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x5E8,0x210}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 500}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x5E8,0x278}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 9999}})
local tt = {0x64DE70,0x30,0x88}
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x2C0}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 16, value = 0, freeze = true}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x28C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 16, value = 0, freeze = true}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x2F4}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 16, value = 0, freeze = true}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x270}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 16, value = 0, freeze = true}})
local tt = {0x64DE70,0x30,0x20,0x338,0x824}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 0.6}})
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = -34564993024.0,
			freeze = (not (25083~=25083)),
		},
	})
end
			string.toMusic("开启成功")
			end
			   , function()

local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x5E8,0x210}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 500}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x5E8,0x278}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 8192}})
local tt = {0x64DE70,0x30,0x88}
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x2C0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 0}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x28C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 0}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x2F4}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 0}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x670,0x270}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 0}})
local tt = {0x64DE70,0x30,0x20,0x338,0x824}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 1}})
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = 8.50279631158571E-21,
			freeze = (not (25083~=25083)),
		},
	})
end
			string.toMusic("关闭成功")	
end),

				changan.switch(
			"蹲趴判断",
			function()
while true do
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0xE98}
local ttt = S_Pointer(t, tt, true)
local  co=gg.getValues({{address=ttt,flags=16,value=nil}})
if co[1].value== 344.25 then---开启
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = -34564993024.0,
			freeze = (not (25083~=25083)),
		},
	})
end
local tt = {0x631CB0,0x30,0x558,0x2568}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 20000}})
local tt = {0x631CB0,0x30,0x558,0x19D8,0x16C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 20000}})
local tt = {0x631CB0,0x30,0x88}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 20000}})
local tt = {0x631CB0,0x30,0x558,0x19D8,0x16C+0x4}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 4, value = 0}})
local tt = {0x631CB0,0x7C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 4, value = 3 }})
local tt = {0x631CB0,0x30,0x88}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 999}})
end
if co[1].value== 120 then---关闭
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = 8.50279631158571E-21,
			freeze = (not (25083~=25083)),
		},
	})
end
end
end
			end,
			function()
				
			end),					
	
		changan.switch("攀爬穿墙",
function() 
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x672330, 0x8, 0x8, 0xF0, 0x1E0}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 16, value = 1000, freeze = true}})
		string.toMusic("开启成功")
end),
		changan.switch(
			"广播体操",
			function()
							so=gg.getRangesList('libUE4.so')[1].start
py=0xBD82A24
setvalue(so+py,4, 0)
					end,
					function()
				so=gg.getRangesList('libUE4.so')[1].start
py=0xBD82A24
setvalue(so+py,4, 939524352)
			end,
			function()
				
			end),				
		changan.switch("打击特效", function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x418,0x50,0x268,0x47C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 9999}})
        string.toMusic("开启成功")
      end
      , function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x418,0x50,0x268,0x47C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 5}})
        string.toMusic("关闭成功")
      end
      ),      						    		
				changan.switch(
			"秒拾取",
			function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x1E03F8, 0x140, 0x68, 0x108, 0x1E0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 150}})

local t = {"libUE4.so:bss", "Cb"}
local tt = {0x6226F0, 0x48, 0x50, 0x8, 0x1E0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 150}})

local t = {"libUE4.so:bss", "Cb"}
local tt = {0x631A90, 0x118, 0x58, 0x8, 0x1E0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 150}})

local t = {"libUE4.so:bss", "Cb"}
local tt = {0x631A90, 0x108, 0x58, 0x8, 0x1E0}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 150}})
			end,
			function()
				
			end),					
				
			changan.switch(
			"一键开启",
			function()
			local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x1A30,0x16C}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address=ttt,flags=16,value=999999}})	

local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x2670}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address=ttt,flags=16,value=999999}})	
			
	
	local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0x1A30,0x170}
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address=ttt,flags=4,value=0}})
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x64DE70,0x30,0x560,0xf70,0x2E4}--广角
local ttt = S_Pointer(t, tt, true)
gg.setValues({{address = ttt, flags = 16, value = 120 }})
			             while "libUE4.so:bss" do
        if gg.getValues({
          {
            address = S_Pointer({
              "libUE4.so:bss",
              "Cb"
            }, {
              6610544,
              48,
              1376,
              1208,
              8,
              1720,
              316
            }, true),
            flags = 16,
            value = nil
          }
        })[1].value == 30 then
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = -34564993024.0,
			freeze = (not (25083~=25083)),
		},
	})
end
          gg.setValues({
            {
              address = S_Pointer({
                "libUE4.so:bss",
                "Cb"
              }, {
                6610544,
                48,
                1376,
                3952,
                740
              }, true),
              flags = 16,
              value = 120
            }
          })
        end
        if gg.getValues({
          {
            address = S_Pointer({
              "libUE4.so:bss",
              "Cb"
            }, {
              6610544,
              48,
              1376,
              1208,
              8,
              1720,
              316
            }, true),
            flags = 16,
            value = nil
          }
        })[1].value == -30 then
if gg.getRangesList("libUE4.so")[1] then
	local t = {}
	t[1] = gg.getRangesList("libUE4.so")[1]["start"] + 0x98208CC; -- 数值地址:0x73824458CC
	gg.addListItems({
		[1] = { 
			address = t[1],
			flags = 16,
			value = 8.50279631158571E-21,
			freeze = (not (25083~=25083)),
		},
	})
end
        end
      end
			end,
			function()
				
			end),							
	},{--第二页
	changan.button("美化功能区",
				function()
				
				end),
				changan.switch(
			"双截棍",
			function()
local t = {"libUE4.so:bss", "Cb"}
local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9807007, freeze = true}})
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
			
			end),
				changan.switch("手弩·银星",
				function()
   local t = {"libUE4.so:bss", "Cb"}
   local tt = {0x63B658, 0x20, 0x66C}
   local ttt = S_Pointer(t, tt, true)
   gg.setValues({{address = ttt, flags = 4, value = 9828009}})

string.toMusic('开启成功')---语音
end,
function()
end),						
			changan.switch("7级头盔·墨守",			
				function()

   local t = {"libUE4.so:bss", "Cb"}
local tt = {0x63B658, 0x20, 0x66C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 9804026, freeze = true}})

string.toMusic('开启成功')---语音
end,
function()
end),
changan.switch("年兽狗砸",			
				function()

   local t = {"libUE4.so:bss", "Cb"}
local tt = {0x63B658, 0x20, 0x66C}
local ttt = S_Pointer(t, tt, true)
gg.addListItems({{address = ttt, flags = 4, value = 9812932, freeze = true}})

string.toMusic('开启成功')---语音
end,
function()
end),								
			changan.switch(
			"蝴蝶刀",
			function()
  local t = {"libUE4.so:bss", "Cb"}
 local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
 local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9805099, freeze = true}})
 gg.clearList()
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
			
			end),
				changan.switch(
			"火焰刀",
			function()
local t = {"libUE4.so:bss", "Cb"}
 local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
 local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9807005, freeze = true}})
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
				
			end),
		changan.switch(
			"闪光盾",
			function()
local t = {"libUE4.so:bss", "Cb"}
 local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
 local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9807006, freeze = true}})
 gg.clearList()
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
				
			end),
				changan.switch(
			"爆炸猎弓",
			function()
			
local t = {"libUE4.so:bss", "Cb"}
 local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
 local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9809010, freeze = true}})
 gg.clearList()
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
				
			end),
				changan.switch(
			"雪隼",
			function()
	local t = {"libUE4.so:bss", "Cb"}
	    local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
	    local ttt = S_Pointer(t, tt, true)
	    gg.addListItems({{address = ttt, flags = 4, value = 9810043, freeze = true}})
	    gg.clearList()
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
				
			end),
				changan.switch(
			"特劳斯",
			function()
local t = {"libUE4.so:bss", "Cb"}
 local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
 local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9805099, freeze = true}})
 gg.clearList()
gg.alert("开启成功")
string.toMusic('开启成功')---语音
			end,
			function()
				
			end),
				changan.switch(
			"卡德尔",
			function()
  local t = {"libUE4.so:bss", "Cb"}
 local tt = {0x67AAE0,0x818,0x748,0x8,0x600,0x0,0x66C}
 local ttt = S_Pointer(t, tt, true)
 gg.addListItems({{address = ttt, flags = 4, value = 9812092, freeze = true}})
 gg.clearList()
gg.alert("原神开启失败")
string.toMusic('开启失败')---语音
			end,
			function()
				
			end),
		

			changan.switch("人物变身",
	    function()
	    so=gg.getRangesList('libUE4.so')[1].start
py=0XB9567C0
setvalue(so+py,16, 1.1)
wc = "人物变身开启成功"
  t = nil
  gg.playMusic("https://fanyi.baidu.com/gettts?lan=zh&text=" .. wc .. "&spd=5&source=wise")	
end),

changan.switch("撬棍改火焰刀（大厅展示）",
	    function()
	    gg.clearResults()
	 gg.setRanges(32)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("9807005", gg.TYPE_DWORD) 
end),
changan.switch("撬棍改双截棍（大厅展示）",
	    function()
	    gg.clearResults()
	 gg.setRanges(32)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("9807007", gg.TYPE_DWORD) 
end),
changan.switch("大砍刀改蝴蝶刀（大厅展示）",
	    function()
	    gg.clearResults()
	 gg.setRanges(32)
	 gg.searchNumber("108001", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("108001", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("9807006", gg.TYPE_DWORD)
end),
changan.switch("撬棍改手弩（大厅展示）",
	    function()
	    gg.clearResults()
	 gg.setRanges(32)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("9828009", gg.TYPE_DWORD) 
end),
changan.switch("撬棍改年兽笨龙（大厅展示）",
	    function()
	    gg.clearResults()
	 gg.setRanges(32)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("108002", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("9812932", gg.TYPE_DWORD) 
end),
changan.switch("赤羽笨龙（衣服）",
function()
gg.clearResults()
gg.setRanges(32)
gg.searchNumber('403251',gg.TYPE_DWORD,false,gg.SIGN_EQUAL,0, -1)
gg.searchNumber('403251',gg.TYPE_DWORD,false,gg.SIGN_EQUAL,0, -1)
gg.getResults(100)
gg.editAll('403888',gg.TYPE_DWORD)
end),

changan.switch("👑（动作）",
function()
gg.searchNumber("2200801", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("2200801", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("2202608", gg.TYPE_DWORD)
	 end),
	 
changan.switch("巅峰宗师特效（动作）",
function()
gg.clearResults()
	 gg.setRanges(32)
	 gg.searchNumber("2201401", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.searchNumber("2201401", gg.TYPE_DWORD, false, gg.SIGN_EQUAL, 0, -1)
	 gg.getResults(100)
	 gg.editAll("2200990", gg.TYPE_DWORD)
	 end),
	 




					
		},{--第5页
changan.button("搜索音乐",
	    function()
	    search = gg.prompt({
"输入要搜索的歌曲\n可加上歌手名字",
"设置显示数量(数字)",
},g.sel,{
"text",
})
if not search then return end
gg.saveVariable(search,g.config)
bei()
go1=search[1]
go3=search[2]
jg=start(go1,go3)
if jg.code == 200 then
fh=jg.content
fh=json(fh)
--print(fh)
Play(gqlb,idb)
else
function inspect()
gg.alert("访问网络异常，错误代码：114514\n\n"..jg.code)
end
if not pcall(inspect) then print("网络异常，请先TMD连接上网络") os.exit() end
end
XGCK=-1
end),
changan.button('关闭音乐',
function()
string.toMusic('音乐关闭成功')
end),



changan.switch("我怀念的",
function()
	gg.playMusic("https://pan.jl8.top/view.php/798ae2d4aae94475f561b6f045df9b0c.mp3")
			    end),          

changan.switch("隐形的翅膀",
function()
	gg.playMusic("https://pan.jl8.top/view.php/28874644d3106ce7abacd84125da5fb7.mp3")
			    end),  
			    
changan.switch("有爱就不怕",
function()
	gg.playMusic("https://pan.jl8.top/view.php/4c13abf05e6b45635ca278f9d0062271.mp3")
			    end),  
			    
changan.switch("恶作剧",
function()
	gg.playMusic("https://pan.jl8.top/view.php/cd62bb7ec601e49a8fbe54a5248ddcba.ogg")
			    end),   
			    
changan.switch("我知道",
function()
	gg.playMusic("https://pan.jl8.top/view.php/20c777325143f51cc39995b933fcbe76.mp3")
			    end),          

changan.switch("慢慢",
function()
	gg.playMusic("https://pan.jl8.top/view.php/c8a3fb129dba251123d2b653643b6aea.ogg")
			    end),                    
			    
changan.switch("我走后",
function()
	gg.playMusic("https://pan.jl8.top/view.php/6a59ef98cd600544e8e6ec90b733828d.mp3")
			    end),  
			    
			   
changan.switch("看片",
function()
gg.toast("不是你看NM呢")
string.toMusic("不是你看NM呢")	
			    end),             
			                     
		},{--第6页，没有写菜单标题所以不显示
			
		},
	
	
	})
--配置表添加表即可加页数，需要与上边菜单标题数对应


if ylfunc~=nil then ylfunc() end
while true do
if tuichu==1 then break end
if 音量键 then
jianting3(qiehuan)
end gg.sleep(300)
end
luajava.setFloatingWindowHide(false)
