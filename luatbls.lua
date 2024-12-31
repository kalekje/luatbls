--% Kale Ewasiuk (kalekje@gmail.com)
--% +REVDATE+
--% Copyright (C) 2025 Kale Ewasiuk
--%
--% Permission is hereby granted, free of charge, to any person obtaining a copy
--% of this software and associated documentation files (the "Software"), to deal
--% in the Software without restriction, including without limitation the rights
--% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--% copies of the Software, and to permit persons to whom the Software is
--% furnished to do so, subject to the following conditions:
--%
--% The above copyright notice and this permission notice shall be included in
--% all copies or substantial portions of the Software.
--%
--% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
--% ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
--% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--% PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT
--% SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
--% ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
--% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
--% OR OTHER DEALINGS IN THE SOFTWARE.


local luatbls = {}

luatbls._luakeys = require'luakeys'()

luatbls._rec_tbl = ''
luatbls._rec_tbl_opts = {}

luatbls._xysep = '%s+' -- spaces separate x-y coordinates
luatbls._tblv = '<v>'
luatbls._tblk = '<k>'
luatbls._cspfx = 'dtbl'
luatbls._debug = false

function luatbls._dprint(s1, s2)
    if luatbls._debug then
        penlight.wrth(s1, s1)
    end
end


setmetatable(luatbls, {__call=function(t,s)return t._get_tbl_item(s) end})

function luatbls._get_tbl_name(s)
    s = s:strip()
    if s == '' then
        return luatbls._rec_tbl
    end
    for _, delim in ipairs{'.', '/', '|'} do
        s = s:split(delim)[1] -- if tbl reference had a . | or /, an indexer was used
    end
    if luatbls[s] == nil then
        local validtbls = penlight.List(penlight.tablex.kkeys(luatbls)):filter(function(s) return not s:startswith('_')  end):join(', ')
        penlight.tex.pkgerror('luatbls', 'Tried to access undefined tbl: "'..s..'".   Valid tbls are: '..validtbls)
        return luatbls._rec_tbl
    end
    return s
end

function luatbls._get_tbl(s)
    s = luatbls._get_tbl_name(s)
    return luatbls[s]
end

function luatbls._get_tbl_index(s, undec)
    undec = undec or false -- flag for allowing undeclared indexing
    local tbl = ''
    local key = ''
    local s_raw = s
    if s:find('%.') then
        local tt = s:split('.')
        tbl = tt[1]
        key = tt[2]
    elseif s:find('/') then
        local tt = s:split('/')
        tbl = tt[1]
        if tbl == '' then tbl = luatbls._rec_tbl end
        key = tonumber(tonumber(tt[2]))
        if key < 0 then key = #luatbls[tbl]+1+key end
    else
        tbl = luatbls._rec_tbl
        key = tonumber(s) or s
        if type(key) == 'number' and key < 0 then key = #luatbls[tbl]+1+key end
    end
    if tbl == '' then tbl = luatbls._rec_tbl end

    if (luatbls[tbl] == nil) or ((not undec) and (luatbls[tbl][key] == nil)) then
        penlight.tex.pkgerror('luatbls',  'Invalid tbl index attempt using: "'..s_raw..'". We tried to use tbl="' ..tbl..'" and key="'..key..'"'..
                'Note that "|" is forbidden here. The recent table is:  '..luatbls._rec_tbl)
    end
    return tbl, key
end

function luatbls._get_tbl_seq(s)
    local tblseq = s:split('|')
    local tbl = nil
    local seq = nil
    if #tblseq == 1 then
        tbl = luatbls._get_tbl_name('')
        seq = tblseq[1]
        if seq == '' then seq = ':,*' end
    elseif #tblseq == 2 then
        tbl = luatbls._get_tbl_name(tblseq[1])
        seq = tblseq[2]
        if seq == '' then seq = ':,*' end
    end
    return tbl, seq
end



function luatbls._get_tbl_item(s, p) -- get item with string, p means print value
  p = p or false
  local tbl, key = luatbls._get_tbl_index(s)
  local itm = luatbls[tbl][key]
  if p then
    tex.sprint(tostring(itm))
  end
  return itm
end


function luatbls._set_tbl_item(s, v)
  tbl, key = luatbls._get_tbl_index(s)
  luatbls[tbl][key] = v
end

function luatbls._check_recent_tbl_undefault()
    local undefaults = {}
    if luatbls._rec_tbl_opts ~= nil then
        local defaults = penlight.tablex.union(
                luatbls._rec_tbl_opts.defs or {},
                luatbls._rec_tbl_opts.defaults or {}
        )
        for k, v in pairs(luatbls[luatbls._rec_tbl]) do
            if defaults[k] == nil then
                undefaults[#undefaults+1] = k
            end
        end
        if penlight.hasval(undefaults) then
            penlight.tex.pkgerror('luatbls',
                    'Invalid keys passed to tbl keyval:  ' .. (', '):join(undefaults) ..
                    ' ;   choices are:  ' .. (', '):join(penlight.tablex.keys(defaults))
            )
        end
    end
end

function luatbls._make_alpha_key(k)
     if tonumber(k) ~= nil then
         k = penlight.Char(tonumber(k))
     end
    return k
end

function luatbls._def_tbl(ind, def, g)
  local _tbl, _key = luatbls._get_tbl_index(ind)
   if def == '' then def = luatbls._cspfx.._tbl..luatbls._make_alpha_key(_key) end
  luatbls._def_tbl_one(luatbls[_tbl][_key], def, g)
end


function luatbls._def_tbl_some(tblseq, def, g)
  for t, k, v in luatbls._iter_tbls_vals(tblseq) do
      if def == '' then def = luatbls._cspfx..t end
      local def2 = def .. luatbls._make_alpha_key(k)
         luatbls._def_tbl_one(v, def2, g)
     end
end

function luatbls._def_tbl_one(v, cs, g)
    if type(v) == 'table' then
        for kk, vv in pairs(v) do
            kk = luatbls._make_alpha_key(kk)
            token.set_macro(cs..kk, tostring(vv), g)
        end
    else
        token.set_macro(cs, tostring(v), g)
    end
end

function luatbls._def_tbl_coords(ind, def)
    local tbl, key = luatbls._get_tbl_index(ind)
    local str = luatbls[tbl][key]
    if def == '' then def = luatbls._cspfx..tbl..key end
    local x, y = str:strip():splitv(luatbls._xysep)
     if (not penlight.hasval(x)) or (not penlight.hasval(y))  then
       penlight.tex.pkgerror('luatbls', '_def_tbl_coords function could not parse coordiantes given as "'..str..'" ensure two numbers separated by space are given!', '', true)
     end
    token.set_macro(def..'x', tostring(x))
    token.set_macro(def..'y', tostring(y))
end


function luatbls._for_tbl_prt(k, v,cmd)
    local cmd_new = cmd:gsub(luatbls._tblv, tostring(v)):gsub(luatbls._tblk, tostring(k)):gsub('(\\%w+) ', '%1') -- for some reason a space gets added to \cs, maybe
    luatbls._dprint(cmd_new, '_for_tbl replacement')
    tex.sprint(cmd_new)
end

function luatbls._for_tbl(tblseq, cmd)
  for t, k, v in luatbls._iter_tbls_vals(tblseq) do
        luatbls._for_tbl_prt(k, v,cmd)
  end
end

function luatbls._for_tbl_e(tbl, cmd)
  for k, v in pairs(tbl) do
        luatbls._for_tbl_prt(k, v,cmd)
  end
end


function luatbls._iter_tbls_vals(s)
    if s:find('|') or ((s:find('%.') == nil) and (s:find('/') == nil))  then
        local tbl, seq = luatbls._get_tbl_seq(s)
        local keyval = {}
        for key, val in penlight.seq.tbltrain(luatbls._get_tbl(tbl), seq) do
         -- todo this should check validity of keys for sequences
            keyval[#keyval+1] = {key, val}
        end
        luatbls._dprint(keyval, 'luatbls._iter_tbls_vals is iterating through tbl: '..tbl..' with sequence:  '..seq)
        local i = 0
        return function()
            i = i + 1
            if i <= #keyval then
                return tbl, keyval[i][1], keyval[i][2]
            end
        end
    else
        local tbl, key = luatbls._get_tbl_index(s)
        local val = luatbls[tbl][key]
        luatbls._dprint(keyval, 'luatbls._iter_tbls_vals is using tbl: '..tbl..' with key: '..key)
        local i = 1
        return function()
            if i == 1 then
                i = i + 1 -- only return the single value
                return tbl, key, val
            end
        end
    end
end

function luatbls._make_args(s, key, val)
    local args = {val}
    if s == nil then
        return args
    end
    s = s:split(')')[1]
    args = luatbls._luakeys.parse(s, {naked_as_value=true})
    for i, v in ipairs(args) do
        if v == luatbls._tblv then
            args[i] = val
        elseif v == luatbls._tblk then
            args[i] = key
        end
    end
    return args
end

function luatbls._make_func(f, key, val)
    f = f:strip()
    fargs = f:split('(')
    f = fargs[1]
    args = luatbls._make_args(fargs[2], key, val)
    if f:startswith(':') then
        f = f:sub(2,-1)
        if type(val) == 'string' then
            f = string[f]
        elseif type(val) == 'table' then
            f = penlight.tablex[f]
        end
    else
        f = penlight._Gdot(f)
    end
    --luatbls._dprint() -- todo more debug printing
    return f, args
end



function luatbls._make_newtbl(tblind, newtbl)
    if tblind == '' then tblind = luatbls._rec_tbl..'|' end
    if newtbl ~= '' then -- determine if new tbl is needed
        local ogtbl = luatbls._get_tbl_name(tblind)
        luatbls[newtbl] = penlight.tablex.deepcopy(luatbls[ogtbl])
        luatbls._rec_tbl = newtbl
        tblind, _ = string.gsub(tblind, ogtbl, newtbl, 1)
    end
    return tblind
end

function luatbls._apply_tbl(tblind, func, newtbl)
    tblind = luatbls._make_newtbl(tblind, newtbl)
    for _, f in pairs(func:split('|')) do
        for tbl, key, val in luatbls._iter_tbls_vals(tblind) do
            local thefunc, args = luatbls._make_func(f, key, val)
            if thefunc == nil then
                penlight.tex.pkgerror('luatbls', 'Tried to apply function: "'..f..'" to tbl value. It yielded no function. Ensure : is used for self-methods')
            end
            luatbls[tbl][key] = thefunc(penlight.utils.unpack(args))
        end
    end
end

return luatbls