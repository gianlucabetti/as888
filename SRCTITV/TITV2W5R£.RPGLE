000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200140120     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500140117     FEDIVABwr  O    E             DISK    usropn
000600140117     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600140117     D edivabds      e ds                  extname(edivab0f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600161122     D depspe          s              5    INZ(*blanks)
002700161122     D curspe          s              5    INZ(*blanks)
002800000830
002900041025     D*------------------
003000041025     D* DS REPERIMENTO NUMERATORE
003100041025     D*------------------
003200041025     D trul33ds      e ds                  inz
003300041025     D*------------------
003400041025     D* DS ARCHITETTURA
003500041025     D*------------------
003600041025     D kpjba         e ds                  inz
003700041025     D*------------------
003800990908
003900010201
004000010201
004100000913     C                   reset                   rrnum
004200990921     C                   reset                   esito
004300990921     C                   reset                   wrkesito
004400000613     C*
004500150213     C                   EXSR      DEFCAM
004600150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004700070719     C*
004800070719     C* Esegue lancio TISI95R solo x chiusura
004900070719     C                   CLEAR                   TISI95DS
005000070719     C                   EVAL      I95TLA = 'C'
005100070719     C                   CALL      'TISI95R'
005200070719     C                   PARM                    TISI95DS
005300000613     C*
005400010202     C* Effettuo la chiamata al CLLE preposto
005500140922     C                   call(e)   'TITVEVTC'
005600140922     C                   parm                    parccm
005700140922     C                   parm                    parmbr
005800140922     C                   parm      '2'           paropz
005900000616     C*
006000010201     C                   seton                                        LR
006100990908
006200000801
006300910830     C*--------------------------------------------------------
006400140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006500910830     C*--------------------------------------------------------
006600040526     C     RWFILE        BEGSR
006700990910     C*
006800990914     C                   if        not %open(tivin00r)
006900990908     C                   open      tivin00r
007000990914     C                   endif
007100140117     C                   if        not %open(edivabwr)
007200140117     C                   open      edivabwr
007300990914     C                   endif
007400140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007500020305     C                   exsr      prevat
007600010201     C*
007700010202     C                   if        chkcall = '0'
007800010202     C*
007900140117     C                   if        not %open(edivatwr)
008000140117     C                   open      edivatwr
008100010201     C                   endif
008200990910     C*
008300010201     C                   clear                   §CTROKVB          5 0
008400020305     C                   clear                   §CTROKVT          5 0
008500000801     C                   clear                   §CTRMO            5 0
008600000801     C                   clear                   §CTRNO            5 0
008700140117     C*
008800140902     C                   if        *in53
008900140117     C                   call      'TIS7P2SER'
009000140117     C                   parm      'O'           pIn_Opz           1
009100140117     C                   parm                    tivlrds
009200140117     C                   parm                    EDIVABDS
009300140117     C                   parm      *blanks       pIn_CdIdAz        2
009400140117     C                   parm      *blanks       pIn_MaskPDF      50
009500140117     C                   parm      *blanks       pOut_Esito        1
009600140902     C                   endif
009700161003     C*
009800130708     C                   SETOFF                                       31
009900921023     C                   DO        *HIVAL
010000990913     C*
010100990915     C                   READ      tivin00r                               70
010200040910     C                   if        vindta > *blanks
010300000613     C                   add       1             rrnum
010400000801     C*
010500000801     C                   if        *in70 = *off
010600000801     C                             and
010700000801     C                             (vinflg = *blanks
010800000801     C                              or vinflg = '0'
010900000801     C                              or vinflg = '2')
011000000801     C*
011100000801     C                   clear                   vinmsg
011200000801     C                   eval      vinflg = '1'
011300040910     C*
011400040910     C* Eseguo routine d traduzione
011500040910     C                   exsr      impvabvat
011600161014     C*
011700161014     C* Scarico buffer
011800161122     C                   IF        *IN55
011900161014     C  N31              exsr      CHKWRI
012000161014     C  N31              WRITE     EDIVAB00
012100161122     C                   ENDIF
012200161014     C                   SETOFF                                       31
012300040802     C*
012400010305     C                   endif
012500000905     C*
012600000905     C                   else
012700000905     C                   eval      vinflg = '1'
012800000905     C                   endif
012900000905     C*
013000000905     C  N70              update    tivin000
013100000905     C*
013200991022     C  N70              ENDdo
013300140117     C*
013400140902     C                   if        *in53
013500140117     C                   call      'TIS7P2SER'
013600140117     C                   parm      'C'           pIn_Opz           1
013700140117     C                   parm                    tivlrds
013800140117     C                   parm                    EDIVABDS
013900140117     C                   parm      *blanks       pIn_CdIdAz        2
014000140117     C                   parm      *blanks       pIn_MaskPDF      50
014100140117     C                   parm      *blanks       pOut_Esito        1
014200140902     C                   endif
014300010202     C*
014400010202     C                   endif
014500990910
014600990910     C* Se non ci sono record con errori ...
014700000710     C                   if        §ctrno = 0
014800990910     C* ... restituisco esito OK.
014900990921     C                   eval      wrkesito = '0'
015000990910     C                   else
015100010201     C                   if        §ctrokvb > 0
015200990921     C                   eval      wrkesito = '1'
015300000710     C                   else
015400000710     C                   eval      wrkesito = '2'
015500990910     C                   endif
015600000710     C                   endif
015700990910     C*
015800990914     C                   if        %open(tivin00r)
015900990908     C                   close     tivin00r
016000990914     C                   endif
016100140117     C                   if        %open(edivabwr)
016200140117     C                   close     edivabwr
016300990914     C                   endif
016400140117     C                   if        %open(edivatwr)
016500140117     C                   close     edivatwr
016600010201     C                   endif
016700990910     C*
016800010201     C                   if        §ctrokvb > 0
016900000724     C                             and vlrpoi <> *zeros
017000010202     C                   exsr      invio
017100990920     C                   endif
017200990920     C*
017300910830     C                   ENDSR
017400000613     C***
017500140922
017600140922     C*----------------------------------------------------*
017700140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017800140922     C*----------------------------------------------------*
017900140922     C     CHKWRI        BEGSR
018000140922     C*
018100140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018200161014     C                   IF        FlgCAS <> '0' OR
018300161014     C                             VABCAS > *zeros
018400140922     C                   IF        VABCBO = '1'
018500140922     C                   EVAL      VABCBO = '4'
018600140922     C                   ELSE
018700140922     C                   EVAL      VABCBO = '6'
018800140922     C                   ENDIF
018900140922     C                   ENDIF
019000140922     C*
019100140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019200140922     C                   EXSR      CHKIMPDIV
019300140922     C*
019400140922     C                   ENDSR
019500990920
019600000801     C*----------------------------------------------------*
019700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019800000801     C*----------------------------------------------------*
019900010201     C     INZVAR        BEGSR
020000000801     C*
020100040802     C                   Z-ADD     *zeros        Num5_0            5 0
020200040802     C                   MOVEL     '0'           FlgCAS            1
020300000801     C*
020400000801     C                   ENDSR
020500000801     C*----------------------------------------------------*
020600040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020700000801     C*----------------------------------------------------*
020800000801     C     DEFCAM        BEGSR
020900000801     C*
021000140117     C                   CLEAR                   EDIVAB00
021100140117     C                   CLEAR                   EDIVAT00
021200020619     C* Imposto i valori di default...
021300161024     C                   Z-ADD     1720441       VABCCM
021400161024     C                   Z-ADD     1720441       VATCCM
021500161024     C                   Z-ADD     172           VABLNP
021600161024     C                   Z-ADD     172           VATLNP
021700160509     C                   Z-ADD     000           VABCTR
021800151119     C                   MOVEL     '7Q'          VABCTM
021900040823     C                   MOVEL     '1'           VABCBO
022000020619     C* ... e poi verifico se sono stati passati come parametri
022100020619     C                   IF        vlrppt > *blanks
022200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABCCM
022700020619     C                   Z-ADD     PiVal         VATCCM
022800020619     C                   ENDIF
022900040506     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABLNP
023500020619     C                   Z-ADD     PiVal         VATLNP
023600040506     C                   ENDIF
023700020619     C                   ENDIF
023800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024000020619     C                   EXSR      CHKNUM
024100020619     C                   IF        PiInt=*on
024200020619     C                   Z-ADD     PiVal         VABCTR
024300040506     C                   ENDIF
024400020619     C                   ENDIF
024500060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024600060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024700060202     C                   ENDIF
024800140902     C                   IF        %subst(vlrppt:16:1) = 'S'
024900140902     C                   SETON                                        53
025000140902     C                   ELSE
025100140902     C                   SETOFF                                       53
025200140902     C                   ENDIF
025300020619     C                   ENDIF
025400000801     C*
025500000801     C                   ENDSR
025600000801     C*----------------------------------------------------*
025700140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025800000801     C*----------------------------------------------------*
025900040910     C     IMPVABVAT     BEGSR
026000161122     C*
026100161122     C                   setoff                                       55
026200161122     C*
026300161122     C* Verifico rittura di codice per "RMN"
026400161122     C                   eval      curspe = %trim(%subst(vindta:239:5))
026500161122     C                   IF        depspe =  *blanks OR
026600161122     C                             depspe <> curspe
026700161122     C                   eval      depspe = curspe
026800161122     C                   seton                                        55
026900040910     C*
027000161014     C* Creo e salvo il numero CMR del file corrente
027100151119     C                   MOVEL     *blanks       savCMR           35
027200161024     C                   EVAL      savCMR = 'NITRO DISTR. - '+
027300161014     C                                      %trim(%char(%date()))
027400100722     C*
027500071210     C* Resetto indicatore d anomalia sul singolo record
027600071210     C                   eval      vinflg = '1'
027700161014     C*
027800071210     C* ......inizializzazioni iniziali e formati record file Bartolini
027900071210     C                   EXSR      INZVAR
028000071210     C                   EXSR      DEFCAM
028100071210     C*
028200071210     C                   Z-ADD     *zeros        errore            1 0
028300071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
028400071210     C                   MOVEL     datcor        VABAAS
028500071210     C                   MOVEL     datcor        VATAAS
028600071210     C                   MOVE      datcor        VABMGS
028700071210     C                   MOVE(P)   vlrpoi        VABFGS
028800071210     C                   MOVE(P)   vlrpoi        VATFGS
028900140903     C*
029000140903     C* Dai "fissi"
029100140903     C                   EVAL      VABCMR = savCMR
029200140903     C                   EVAL      VABDCM = datcor
029300140903     C                   EVAL      VABDTS = datcor
029400140903     C                   EVAL      VABHMS = oracor
029500140903     C                   EVAL      VABCNT = 1
029600140903     C*
029700071210     C* ......VABNSP/VATNSP
029800071210     C* NSP => Stacco un numeratore da AZNUM
029900160609     C                   clear                   TRUL33DS
030000160609     C                   eval      I33OPE = *zeros
030100160609     C                   eval      I33CNU = 302
030200160609     C                   eval      I33NUM = 1
030300160609     C                   movel     TRUL33DS      KPJBU
030400160609     C                   call      'TRUL33R'
030500160609     C                   parm                    KPJBA
030600160609     C                   movel     KPJBU         TRUL33DS
030700160609     C                   if        O33ERR = *zeros
030800160609     C                   z-add     O33NRF        VABNSP
030900160609     C                   z-add     O33NRF        VATNSP
031000160609     C                   else
031100160609     C                   Z-ADD     1             errore
031200160609     C                   EVAL      vinmsg = %trimr(vinmsg)
031300160609     C                             + ' ' + 'VABNSP VATNSP'
031400160609     C                   endif
031500161014     C* ......VABRMA
031600161024     C                   EVAL      VABRMA=%trim(%subst(vindta:239:5))
031700161014     C* ......VABRSD
031800161024     C                   EVAL      VABRSD=%trim(%subst(vindta:98:30))
031900161014     C* ......VABIND
032000161024     C                   EVAL      VABIND=%trim(%subst(vindta:128:30))
032100150611     C* ......VABLOD
032200161024     C                   EVAL      VABLOD=%trim(%subst(vindta:163:30))
032300161014     C* ......VABCAD
032400161024     C                   EVAL      VABCAD=%trim(%subst(vindta:158:5))
032500150611     C* ......VABPRD
032600161024     C                   EVAL      VABPRD=%trim(%subst(vindta:193:2))
032700150611     C* ......VABPRD
032800150611     C* Reperisco la provincia dal CAP e dalla località
032900150611     C                   IF        VABPRD  = *blanks AND
033000150611     C                             VABLOD <> *blanks AND
033100150611     C                             VABCAD <> *blanks AND
033200150611     C                             VABNZD  = *blanks
033300150611     C                   CLEAR                   TISI95DS
033400150611     C                   EVAL      I95TCN = '3'
033500150611     C                   Z-ADD     datcor        I95DAT
033600150611     C                   EVAL      I95CAP = VABCAD
033700150611     C                   EVAL      I95LOC = VABLOD
033800150611     C                   CALL      'TISI95R'
033900150611     C                   PARM                    TISI95DS
034000150611     C                   EVAL      VABPRD = O95PRV
034100150611     C                   ENDIF
034200161024     C* ......VABNOT / VABNT2
034300161024     C                   MOVEL     *blanks       wNOTE            80
034400161024     C                   EVAL      wNOTE =%trim(%subst(vindta:246:80))
034500161024     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
034600161024     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
034700160609     C* ......VABRMN
034800161024     C                   EVAL      PiStr=%trim(%subst(vindta:239:5))
034900160617     C                   EXSR      CHKNUM
035000160617     C                   IF        PiInt=*on
035100160617     C                   Z-ADD     PiVal         VABRMN
035200160617     C                   ELSE
035300160617     C                   Z-ADD     1             VABRMN
035400160617     C                   EVAL      vinmsg = %trimr(vinmsg)
035500160617     C                             + ' ' + 'VABRMN'
035600160617     C                   ENDIF
035700140922     C* ......VATNOT_P
035800150611     C***                if        *in53
035900140922     C*
036000150611     C* Solo se valorizzato RMA
036100150611     C***                if        wRMA_1 <> *blanks
036200150611     C***                eval      pIn_MaskPDF =
036300150611     C***                                  %trim(%subst(vindta:1292:100))
036400150611     C***                call      'TIS7P2SER'
036500150611     C***                parm      'W'           pIn_Opz           1
036600150611     C***                parm                    tivlrds
036700150611     C***                parm                    EDIVABDS
036800150611     C***                parm      '10'          pIn_CdIdAz        2
036900150611     C***                parm                    pIn_MaskPDF      50
037000150611     C***                parm      *blanks       pOut_Esito        1
037100150611     C***                endif
037200150611     C***                endif
037300140117     C*
037400160617     C* ......VABNCL
037500161024     C                   EVAL      PiStr=%trim(%subst(vindta:207:6))
037600161014     C                   EXSR      CHKNUM
037700161014     C                   IF        PiInt=*on
037800161122     C                   Z-ADD     PiVal         VABNCL
037900161014     C                   ELSE
038000161014     C                   Z-ADD     *zeros        VABNCL
038100161014     C                   EVAL      vinmsg = %trimr(vinmsg)
038200161014     C                             + ' ' + 'VABNCL'
038300161014     C                   ENDIF
038400161107     C* ......VABPKB
038500161107     C                   EVAL      PiStr=%trim(%subst(vindta:213:8))
038600161107     C                   EXSR      CHKNUM
038700161107     C                   IF        PiNum=*on
038800161122     C                   Z-ADD     PiVal         VABPKB
038900161107     C                   ELSE
039000161107     C                   Z-ADD     *zeros        VABPKB
039100161107     C                   EVAL      vinmsg = %trimr(vinmsg)
039200161107     C                             + ' ' + 'VABPKB'
039300161107     C                   ENDIF
039400161014     C* ......VABCAS
039500161024     C                   IF        %trim(%subst(vindta:221:13))<>*zeros  AND
039600161024     C                             %trim(%subst(vindta:221:13))<>*blanks AND
039700161024     C                             %trim(%subst(vindta:221:13))<>'0000000000,00'
039800161024     C                   EVAL      VABVCA='EUR'
039900161024     C                   EVAL      VABTIC='TM'
040000161024     C                   EVAL      PiStr=%trim(%subst(vindta:221:13))
040100161014     C                   EVAL      FlgCAS = '1'
040200161014     C                   EXSR      CHKNUM
040300161014     C                   IF        PiNum=*on
040400161014     C                   Z-ADD     PiVal         VABCAS
040500161014     C                   ELSE
040600161014     C                   Z-ADD     *zeros        VABCAS
040700161014     C                   EVAL      vinmsg = %trimr(vinmsg)
040800161014     C                             + ' ' + 'VABCAS'
040900161014     C                   ENDIF
041000161014     C                   ENDIF
041100161122     C*
041200161122     C                   ENDIF
041300161122     C*
041400161122     C* La rottura di codice "non vale" per il dettaglio segncaolli cliente
041500161122     C*
041600161024     C* ......VATNOT_E =>
041700161123     C                   EVAL      VATNOT=%trim(%subst(vindta:234:15))
041800151119     C                   IF        VATNOT <> *blanks
041900151119     C                   EVAL      VATCMR = VABCMR
042000151119     C                   EVAL      VATCNT = VABCNT
042100151119     C                   EVAL      VATTRC = 'E'
042200151119     C                   ADD       1             §CTROKVT
042300151119     C                   WRITE     EDIVAT00
042400151119     C                   ENDIF
042500140117     C***
042600010202     C*
042700000801     C* Ebbene...
042800000801     C                   ADD       1             §CTRMO
042900010201     C                   IF        errore <> *zeros
043000000801     C                   ADD       1             §CTRNO
043100000801     C                   EVAL      vinflg = '2'
043200000801     C                   ELSE
043300010201     C                   ADD       1             §CTROKVB
043400000801     C                   ENDIF
043500000801     C*
043600000801     C                   ENDSR
043700010202     C*----------------------------------------------------*
043800140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
043900010202     C*----------------------------------------------------*
044000020305     C     PREVAT        BEGSR
044100010202     C*
044200140117     C* Compongo il nome del membro da dare al EDIVATWR
044300010202     C                   eval      parmbr = vlrhdl
044400010202     C                   movel     'M'           parmbr
044500060113     C                   eval      parccm = vlrksc
044600010202     C                   eval      paropz = '1'
044700010202     C* Effettuo la chiamata al CLLE preposto
044800140117     C                   call(e)   'TITVEVTC'
044900010202     C                   parm                    parccm
045000010202     C                   parm                    parmbr
045100010202     C                   parm                    paropz
045200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300010202     C                   if        %error
045400010202     C                   movel     '1'           chkcall
045500010202     C                   else
045600010202     C                   movel     '0'           chkcall
045700010202     C                   endif
045800010202     C*
045900010202     C                   ENDSR
046000000801     C*----------------------------------------------------*
046100000801     C*  CONTROLLO NUMERICITA' CAMPI
046200000801     C*----------------------------------------------------*
046300000801     C     CHKNUM        BEGSR
046400000801     C*
046500000801     C                   call(e)   'ISNUMERIC'
046600000801     C                   PARM                    PiStr            30
046700161107     C                   PARM      ','           PiDecChr          1
046800000801     C                   PARM      *ZEROS        PiVal            30 9
046900000801     C                   PARM      '0'           PiInt             1
047000000801     C                   PARM      '0'           PiNum             1
047100000801     C                   IF        %error
047200000801     C                   EVAL      PiInt=*off
047300000801     C                   ENDIF
047400000801     C*
047500000801     C                   ENDSR
047600000801     C***
047700000801
047800011113
047900011113     C*----------------------------------------------------*
048000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048100011113     C*----------------------------------------------------*
048200011113     C     CHKIMPDIV     BEGSR
048300011113     C*
048400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048500011113     C                   Z-ADD     *zeros        wrkDec            9 9
048600011113     C*
048700011113     C* Come prima cosa effettuo considerazioni sulla divisa
048800011113     C                   IF        vabIAS > *zeros
048900011113     C                   IF        vabVAS <> 'EUR'
049000011113     C                   EVAL      vabVAS =  'ITL'
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C                   IF        vabCAS > *zeros
049500011113     C                   IF        vabVCA <> 'EUR'
049600011113     C                   EVAL      vabVCA =  'ITL'
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C                   IF        vabVMD > *zeros
050100020305     C                   IF        vabVAD <> 'EUR'
050200011113     C                   EVAL      vabVAD =  'ITL'
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050700011113     C                   Z-ADD     vabIAS        wrkDec
050800011113     C                   IF        wrkDec > *zeros
050900011113     C                   IF        vabVAS = 'ITL'
051000011113     C                   EVAL      vabIAS = *zeros
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051500011113     C                   Z-ADD     vabCAS        wrkDec
051600011113     C                   IF        wrkDec > *zeros
051700011113     C                   IF        vabVCA = 'ITL'
051800011113     C                   EVAL      vabCAS = *zeros
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052300011113     C                   Z-ADD     vabVMD        wrkDec
052400011113     C                   IF        wrkDec > *zeros
052500011113     C                   IF        vabVAD = 'ITL'
052600011113     C                   EVAL      vabVMD = *zeros
052700011113     C                   ENDIF
052800011113     C                   ENDIF
052900011113     C*
053000011113     C                   ENDSR
053100011113     C***
053200011113
053300011113
053400000801
053500000801
053600990920      /TITLE Invio dei dati al punto operativo.
053700010202     C     invio         BEGSR
053800990920     C*
053900140117     C* 1° invio EDIVAT
054000010201     C                   reset                   dscmz
054100010201     C                   move      vlrpoi        cmzdst
054200140117     C                   eval      cmzfld = 'EDIVATWR'
054300010201     C                   eval      cmzmbd = vlrhdl
054400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054500021009     C***                if        prmfir = *blanks
054600140117     C                   eval      cmzfla = 'EDIVAT0F'
054700140117     C                   eval      cmzmba = 'EDIVAT0F'
054800021009     C***                else
054900021009     C***                eval      cmzfla = prmfir
055000021009     C***                eval      cmzmba = prmfir
055100021009     C***                endif
055200010201     C                   eval      cmznrr = *zeros
055300020305     C                   move      §ctrokvt      cmznrr
055400021018     C                   eval      cmzlba = vlrfl1
055500010201     C                   call(e)   'TIS711C'
055600010201     C                   parm                    dscmz
055700010201     C                   parm      *blanks       esito
055800010205     C                   if        %error
055900010205     C                             or cmzerr = '1'
056000010205     C                             or esito  = '1'
056100010205     C                   eval      wrkesito = '3'
056200010205     C                   else
056300010201     C*
056400140117     C* 2° invio EDIVAB
056500010201     C                   reset                   dscmz
056600010201     C                   move      vlrpoi        cmzdst
056700010201     C                   eval      cmzfld = vlrfou
056800010201     C                   eval      cmzmbd = vlrhdl
056900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057000021009     C***                if        prmfir = *blanks
057100140117     C                   eval      cmzfla = 'EDIVAB0F'
057200140117     C                   eval      cmzmba = 'EDIVAB0F'
057300021009     C***                else
057400021009     C***                eval      cmzfla = prmfir
057500021009     C***                eval      cmzmba = prmfir
057600021009     C***                endif
057700010201     C                   eval      cmznrr = *zeros
057800010201     C                   move      §ctrokvb      cmznrr
057900021018     C                   eval      cmzlba = vlrfl1
058000010201     C                   call(e)   'TIS711C'
058100010201     C                   parm                    dscmz
058200010201     C                   parm      *blanks       esito
058300010201     C                   if        %error
058400010201     C                             or cmzerr = '1'
058500010201     C                             or esito  = '1'
058600010201     C                   eval      wrkesito = '3'
058700010201     C                   endif
058800010205     C                   endif
058900990920     C*
059000000613     C                   ENDSR
059100000613     C***
059200070411
059300070411     C     *pssr         BEGSR
059400070411     C*
059500070411     C                   if        %open(tivin00r)
059600070411     C                   close     tivin00r
059700070411     C                   endif
059800140117     C                   if        %open(edivabwr)
059900140117     C                   close     edivabwr
060000070411     C                   endif
060100140117     C                   if        %open(edivatwr)
060200140117     C                   close     edivatwr
060300070411     C                   endif
060400070411     C*
060500070411     C* Effettuo la chiamata al CLLE preposto
060600140117     C                   call(e)   'TITVEVTC'
060700070411     C                   parm                    parccm
060800070411     C                   parm                    parmbr
060900070411     C                   parm      '2'           paropz
061000070411     C*
061100070411     C                   eval      wrkesito = '2'
061200070411     C*
061300070411     C                   seton                                        LR
061400070411     C*
061500070411     C                   ENDSR     '*CANCL'
061600070411     C***
061700070411
061800990910
061900000613     C     *inzsr        BEGSR
062000990910     C*
062100990910     C     *entry        plist
062200990920     C                   parm                    tivlrds
062300990921     C                   parm      wrkesito      esito
062400000724     C                   parm                    prmlit
062500000710     C                   parm                    prmfir
062600000613     C*
062700000830     C* CALCOLA LA DATA CORRENTE
062800140117     C                   time                    wn14             14 0
062900140117     C                   movel     wn14          oracor            6 0          *ORA
063000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063100100722     C                   eval      datcor = %dec(%date() : *ISO)
063200000830     C*
063300000613     C                   ENDSR
063400000613     C***
