% Autogenerated with DRAKON Editor 1.30

-module(udh_parser).
-export([pack/1, unpack/1]).

pack(Pdu) ->
    case Pdu of
        #{udh := UDH,
  short_message := SM}
= Pdu ->
            % item 166
            SM1 = unsplit(UDH, SM),
            maps:without(
                [udh, <<"udh">>],
                Pdu#{short_message => SM1}
            )
        ;
        #{<<"udh">> := UDH,
  <<"short_message">> := SM} = Pdu ->
            % item 167
            SM1 = unsplit(UDH, SM),
            maps:without(
                [udh, <<"udh">>],
                Pdu#{<<"short_message">> => SM1}
            )
        ;
        #{udh := UDH, xser := XSERs} = Pdu ->
            % item 168
            Data = unsplit(UDH, []),
            maps:without(
            	[udh, <<"udh">>],
            	Pdu#{xser => [#{type => 1, data => Data} | XSERs]}
            )
        ;
        #{<<"udh">> := UDH, <<"xser">> := XSERs} = Pdu ->
            % item 170
            Data = unsplit(UDH, []),
            maps:without(
            	[udh, <<"udh">>],
            	Pdu#{<<"xser">> => [#{<<"type">> => 1, <<"data">> => Data} | XSERs]}
            )
        ;
        #{udh := _, ot := _} = Pdu ->
            % item 173
            pack(Pdu#{xser => []})
        ;
        #{<<"udh">> := _, <<"ot">> := _} = Pdu ->
            % item 176
            pack(Pdu#{<<"xser">> => []})
        ;
        Pdu ->
            % item 221
            Pdu
    end
.

split(Bin) ->
    case Bin of
        <<5, 0, 3,
  R:8, C:8, N:8,
  Rest/binary>> ->
            % item 102
            {#{bit => 8, len => 5, info_elm_id => 0,
               hdr_len => 3, msg_ref_num => R,
               total_segments => C,
               segment_seqnum => N},
            Rest}
        ;
        [5, 0, 3, R, C, N | Rest] ->
            % item 101
            {#{bit => 8, len => 5, info_elm_id => 0,
               hdr_len => 3, msg_ref_num => R,
               total_segments => C,
               segment_seqnum => N},
            Rest}
        ;
        <<6, 8, 4,
  R:16, C:8, N:8,
  Rest/binary>> ->
            % item 100
            {#{bit => 16, len => 6, info_elm_id => 8,
               hdr_len => 4,
               msg_ref_num => R, total_segments => C,
               segment_seqnum => N},
            Rest}
        ;
        [6, 8, 4, RH, RL, C, N | Rest] ->
            % item 99
            {#{bit => 16, len => 6, info_elm_id => 8,
               hdr_len => 4,
               msg_ref_num => [RH, RL],
               total_segments => C, segment_seqnum => N},
            Rest}
        ;
        Bin ->
            % item 98
            {#{}, Bin}
    end
.

unpack(Pdu) ->
    case Pdu of
        #{short_message := SM0} = Pdu ->
            % item 191
            {UDH, SM} = split(SM0),
            if map_size(UDH) > 1 ->
                    Pdu#{short_message => SM,
                         udh => UDH};
                true ->
                    Pdu#{short_message => SM}
            end
        ;
        #{<<"short_message">> := SM0} = Pdu ->
            % item 192
            {UDH, SM} = split(SM0),
            UDH1 = maps:fold(fun(K, V, M) ->
                M#{atom_to_binary(K, utf8) => V}
            end, #{}, UDH),
            if map_size(UDH) > 1 ->
                    Pdu#{<<"short_message">> => SM,
                         <<"udh">> => UDH1};
                true ->
                    Pdu#{<<"short_message">> => SM}
            end
        ;
        #{xser := XSERs} = Pdu ->
            case {xser(XSERs, Pdu), Pdu} of
                {NewPdu, #{xser := []}} ->
                    % item 200
                    NewPdu
                ;
                {#{xser := []} = NewPdu, Pdu} ->
                    % item 201
                    maps:without([xser], NewPdu)
                ;
                {NewPdu, Pdu} ->
                    % item 202
                    NewPdu
            end
        ;
        #{<<"xser">> := XSERs} = Pdu ->
            case {xser(XSERs, Pdu), Pdu} of
                {NewPdu, #{<<"xser">> := []}} ->
                    % item 212
                    NewPdu
                ;
                {#{<<"xser">> := []} = NewPdu, Pdu} ->
                    % item 213
                    maps:without([<<"xser">>], NewPdu)
                ;
                {NewPdu, Pdu} ->
                    % item 214
                    NewPdu
            end
        ;
        Pdu ->
            % item 216
            Pdu
    end
.

unsplit(Pdu, SM) ->
    case Pdu of
        #{<<"bit">> := 8, <<"len">> := 5,
   <<"info_elm_id">> := 0,
   <<"hdr_len">> := 3,
   <<"msg_ref_num">> := R,
   <<"total_segments">> := C, 
   <<"segment_seqnum">> := N} ->
            % item 116
            case is_list(SM) of true -> 
                % item 119
                [5, 0, 3, R, C, N | SM]
            ; false ->
                % item 120
                <<5, 0, 3, R:8, C:8, N:8, SM/binary>>
            end
        ;
        #{bit := 8, len := 5, info_elm_id := 0,
   hdr_len := 3, msg_ref_num := R,
   total_segments := C,
   segment_seqnum := N} ->
            % item 121
            case is_list(SM) of true -> 
                % item 122
                [5, 0, 3, R, C, N | SM]
            ; false ->
                % item 123
                <<5, 0, 3, R:8, C:8, N:8, SM/binary>>
            end
        ;
        #{<<"bit">> := 16, <<"len">> := 6,
  <<"info_elm_id">> := 8,
  <<"hdr_len">> := 4,
  <<"msg_ref_num">> := R,
  <<"total_segments">> := C,
  <<"segment_seqnum">> := N} ->
            % item 125
            case is_list(SM) of true -> 
                % item 126
                lists:flatten([6, 8, 4, R, C, N | SM])
            ; false ->
                % item 127
                <<6, 8, 4, R:16, C:8, N:8, SM/binary>>
            end
        ;
        #{bit := 16, len := 6,
  info_elm_id := 8,
  hdr_len := 4,
  msg_ref_num := R,
  total_segments := C,
  segment_seqnum := N} ->
            % item 130
            case is_list(SM) of true -> 
                % item 132
                lists:flatten([6, 8, 4, R, C, N | SM])
            ; false ->
                % item 133
                <<6, 8, 4, R:16, C:8, N:8, SM/binary>>
            end
    end
.

xser(XSERs, Pdu) ->
    case {XSERs, Pdu} of
        {XSERs, Pdu} when is_list(XSERs) ->
            % item 147
            lists:foldl(fun xser/2, Pdu, XSERs)
        ;
        {#{type := 1, data := Data} = XSER,
 #{xser := NewXSREs} = PduMap} ->
            % item 148
            {UDH, []} = split(Data),
            PduMap#{udh => UDH,
                    xser => NewXSREs -- [XSER]}
        ;
        {#{<<"type">> := 1, <<"data">> := Data} = XSER,
 #{<<"xser">> := NewXSREs} = PduMap} ->
            % item 149
            {UDH0, []} = split(Data),
            UDH = maps:fold(
                fun(K, V, M) ->
                    M#{atom_to_binary(K, utf8) => V}
                end, #{}, UDH0),
            PduMap#{<<"udh">> => UDH,
                    <<"xser">> => NewXSREs -- [XSER]}
        ;
        {_, PduConst} ->
            % item 219
            PduConst
    end
.


