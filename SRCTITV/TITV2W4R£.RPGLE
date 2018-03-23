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
002600000830
002700041025     D*------------------
002800041025     D* DS REPERIMENTO NUMERATORE
002900041025     D*------------------
003000041025     D trul33ds      e ds                  inz
003100041025     D*------------------
003200041025     D* DS ARCHITETTURA
003300041025     D*------------------
003400041025     D kpjba         e ds                  inz
003500041025     D*------------------
003600990908
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300150213     C                   EXSR      DEFCAM
004400150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004500070719     C*
004600070719     C* Esegue lancio TISI95R solo x chiusura
004700070719     C                   CLEAR                   TISI95DS
004800070719     C                   EVAL      I95TLA = 'C'
004900070719     C                   CALL      'TISI95R'
005000070719     C                   PARM                    TISI95DS
005100000613     C*
005200010202     C* Effettuo la chiamata al CLLE preposto
005300140922     C                   call(e)   'TITVEVTC'
005400140922     C                   parm                    parccm
005500140922     C                   parm                    parmbr
005600140922     C                   parm      '2'           paropz
005700000616     C*
005800010201     C                   seton                                        LR
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900140117     C                   if        not %open(edivabwr)
007000140117     C                   open      edivabwr
007100990914     C                   endif
007200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007300020305     C                   exsr      prevat
007400010201     C*
007500010202     C                   if        chkcall = '0'
007600010202     C*
007700140117     C                   if        not %open(edivatwr)
007800140117     C                   open      edivatwr
007900010201     C                   endif
008000990910     C*
008100010201     C                   clear                   §CTROKVB          5 0
008200020305     C                   clear                   §CTROKVT          5 0
008300000801     C                   clear                   §CTRMO            5 0
008400000801     C                   clear                   §CTRNO            5 0
008500140117     C*
008600140902     C                   if        *in53
008700140117     C                   call      'TIS7P2SER'
008800140117     C                   parm      'O'           pIn_Opz           1
008900140117     C                   parm                    tivlrds
009000140117     C                   parm                    EDIVABDS
009100140117     C                   parm      *blanks       pIn_CdIdAz        2
009200140117     C                   parm      *blanks       pIn_MaskPDF      50
009300140117     C                   parm      *blanks       pOut_Esito        1
009400140902     C                   endif
009500161003     C*
009600130708     C                   SETOFF                                       31
009700921023     C                   DO        *HIVAL
009800990913     C*
009900990915     C                   READ      tivin00r                               70
010000040910     C                   if        vindta > *blanks
010100000613     C                   add       1             rrnum
010200000801     C*
010300000801     C                   if        *in70 = *off
010400000801     C                             and
010500000801     C                             (vinflg = *blanks
010600000801     C                              or vinflg = '0'
010700000801     C                              or vinflg = '2')
010800000801     C*
010900000801     C                   clear                   vinmsg
011000000801     C                   eval      vinflg = '1'
011100040910     C*
011200040910     C* Eseguo routine d traduzione
011300040910     C                   exsr      impvabvat
011400161014     C*
011500161014     C* Scarico buffer
011600161014     C  N31              exsr      CHKWRI
011700161014     C  N31              WRITE     EDIVAB00
011800161014     C                   SETOFF                                       31
011900040802     C*
012000010305     C                   endif
012100000905     C*
012200000905     C                   else
012300000905     C                   eval      vinflg = '1'
012400000905     C                   endif
012500000905     C*
012600000905     C  N70              update    tivin000
012700000905     C*
012800991022     C  N70              ENDdo
012900140117     C*
013000140902     C                   if        *in53
013100140117     C                   call      'TIS7P2SER'
013200140117     C                   parm      'C'           pIn_Opz           1
013300140117     C                   parm                    tivlrds
013400140117     C                   parm                    EDIVABDS
013500140117     C                   parm      *blanks       pIn_CdIdAz        2
013600140117     C                   parm      *blanks       pIn_MaskPDF      50
013700140117     C                   parm      *blanks       pOut_Esito        1
013800140902     C                   endif
013900010202     C*
014000010202     C                   endif
014100990910
014200990910     C* Se non ci sono record con errori ...
014300000710     C                   if        §ctrno = 0
014400990910     C* ... restituisco esito OK.
014500990921     C                   eval      wrkesito = '0'
014600990910     C                   else
014700010201     C                   if        §ctrokvb > 0
014800990921     C                   eval      wrkesito = '1'
014900000710     C                   else
015000000710     C                   eval      wrkesito = '2'
015100990910     C                   endif
015200000710     C                   endif
015300990910     C*
015400990914     C                   if        %open(tivin00r)
015500990908     C                   close     tivin00r
015600990914     C                   endif
015700140117     C                   if        %open(edivabwr)
015800140117     C                   close     edivabwr
015900990914     C                   endif
016000140117     C                   if        %open(edivatwr)
016100140117     C                   close     edivatwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   if        §ctrokvb > 0
016500000724     C                             and vlrpoi <> *zeros
016600010202     C                   exsr      invio
016700990920     C                   endif
016800990920     C*
016900910830     C                   ENDSR
017000000613     C***
017100140922
017200140922     C*----------------------------------------------------*
017300140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017400140922     C*----------------------------------------------------*
017500140922     C     CHKWRI        BEGSR
017600140922     C*
017700140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
017800161014     C                   IF        FlgCAS <> '0' OR
017900161014     C                             VABCAS > *zeros
018000140922     C                   IF        VABCBO = '1'
018100140922     C                   EVAL      VABCBO = '4'
018200140922     C                   ELSE
018300140922     C                   EVAL      VABCBO = '6'
018400140922     C                   ENDIF
018500140922     C                   ENDIF
018600140922     C*
018700140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
018800140922     C                   EXSR      CHKIMPDIV
018900140922     C*
019000140922     C                   ENDSR
019100990920
019200000801     C*----------------------------------------------------*
019300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019400000801     C*----------------------------------------------------*
019500010201     C     INZVAR        BEGSR
019600000801     C*
019700040802     C                   Z-ADD     *zeros        Num5_0            5 0
019800040802     C                   MOVEL     '0'           FlgCAS            1
019900000801     C*
020000000801     C                   ENDSR
020100000801     C*----------------------------------------------------*
020200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020300000801     C*----------------------------------------------------*
020400000801     C     DEFCAM        BEGSR
020500000801     C*
020600140117     C                   CLEAR                   EDIVAB00
020700140117     C                   CLEAR                   EDIVAT00
020800020619     C* Imposto i valori di default...
020900161014     C                   Z-ADD     1111489       VABCCM
021000161014     C                   Z-ADD     1111489       VATCCM
021100161003     C                   Z-ADD     111           VABLNP
021200161003     C                   Z-ADD     111           VATLNP
021300160509     C                   Z-ADD     000           VABCTR
021400151119     C                   MOVEL     '7Q'          VABCTM
021500040823     C                   MOVEL     '1'           VABCBO
021600020619     C* ... e poi verifico se sono stati passati come parametri
021700020619     C                   IF        vlrppt > *blanks
021800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022000020619     C                   EXSR      CHKNUM
022100020619     C                   IF        PiInt=*on
022200020619     C                   Z-ADD     PiVal         VABCCM
022300020619     C                   Z-ADD     PiVal         VATCCM
022400020619     C                   ENDIF
022500040506     C                   ENDIF
022600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022800020619     C                   EXSR      CHKNUM
022900020619     C                   IF        PiInt=*on
023000020619     C                   Z-ADD     PiVal         VABLNP
023100020619     C                   Z-ADD     PiVal         VATLNP
023200040506     C                   ENDIF
023300020619     C                   ENDIF
023400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023600020619     C                   EXSR      CHKNUM
023700020619     C                   IF        PiInt=*on
023800020619     C                   Z-ADD     PiVal         VABCTR
023900040506     C                   ENDIF
024000020619     C                   ENDIF
024100060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024200060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024300060202     C                   ENDIF
024400140902     C                   IF        %subst(vlrppt:16:1) = 'S'
024500140902     C                   SETON                                        53
024600140902     C                   ELSE
024700140902     C                   SETOFF                                       53
024800140902     C                   ENDIF
024900020619     C                   ENDIF
025000000801     C*
025100000801     C                   ENDSR
025200000801     C*----------------------------------------------------*
025300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025400000801     C*----------------------------------------------------*
025500040910     C     IMPVABVAT     BEGSR
025600040910     C*
025700161014     C* Creo e salvo il numero CMR del file corrente
025800151119     C                   MOVEL     *blanks       savCMR           35
025900161014     C                   EVAL      savCMR = 'YESFOOD - MAZZOCCO '+
026000161014     C                                      %trim(%char(%date()))
026100100722     C*
026200071210     C* Resetto indicatore d anomalia sul singolo record
026300071210     C                   eval      vinflg = '1'
026400161014     C*
026500071210     C* ......inizializzazioni iniziali e formati record file Bartolini
026600071210     C                   EXSR      INZVAR
026700071210     C                   EXSR      DEFCAM
026800071210     C*
026900071210     C                   Z-ADD     *zeros        errore            1 0
027000071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
027100071210     C                   MOVEL     datcor        VABAAS
027200071210     C                   MOVEL     datcor        VATAAS
027300071210     C                   MOVE      datcor        VABMGS
027400071210     C                   MOVE(P)   vlrpoi        VABFGS
027500071210     C                   MOVE(P)   vlrpoi        VATFGS
027600140903     C*
027700140903     C* Dai "fissi"
027800140903     C                   EVAL      VABCMR = savCMR
027900140903     C                   EVAL      VABDCM = datcor
028000140903     C                   EVAL      VABDTS = datcor
028100140903     C                   EVAL      VABHMS = oracor
028200140903     C                   EVAL      VABCNT = 1
028300140903     C*
028400071210     C* ......VABNSP/VATNSP
028500071210     C* NSP => Stacco un numeratore da AZNUM
028600160609     C                   clear                   TRUL33DS
028700160609     C                   eval      I33OPE = *zeros
028800160609     C                   eval      I33CNU = 302
028900160609     C                   eval      I33NUM = 1
029000160609     C                   movel     TRUL33DS      KPJBU
029100160609     C                   call      'TRUL33R'
029200160609     C                   parm                    KPJBA
029300160609     C                   movel     KPJBU         TRUL33DS
029400160609     C                   if        O33ERR = *zeros
029500160609     C                   z-add     O33NRF        VABNSP
029600160609     C                   z-add     O33NRF        VATNSP
029700160609     C                   else
029800160609     C                   Z-ADD     1             errore
029900160609     C                   EVAL      vinmsg = %trimr(vinmsg)
030000160609     C                             + ' ' + 'VABNSP VATNSP'
030100160609     C                   endif
030200161014     C* ......VABRMA
030300161014     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
030400161014     C* ......VABRSD
030500161014     C                   EVAL      VABRSD=%trim(%subst(vindta:243:30))
030600161014     C* ......VABIND
030700161014     C                   EVAL      VABIND=%trim(%subst(vindta:273:30))
030800150611     C* ......VABLOD
030900161014     C                   EVAL      VABLOD=%trim(%subst(vindta:303:30))
031000161014     C* ......VABCAD
031100161014     C                   EVAL      VABCAD=%trim(%subst(vindta:343:10))
031200150611     C* ......VABPRD
031300161014     C                   EVAL      VABPRD=%trim(%subst(vindta:333:5))
031400150611     C* ......VABPRD
031500150611     C* Reperisco la provincia dal CAP e dalla località
031600150611     C                   IF        VABPRD  = *blanks AND
031700150611     C                             VABLOD <> *blanks AND
031800150611     C                             VABCAD <> *blanks AND
031900150611     C                             VABNZD  = *blanks
032000150611     C                   CLEAR                   TISI95DS
032100150611     C                   EVAL      I95TCN = '3'
032200150611     C                   Z-ADD     datcor        I95DAT
032300150611     C                   EVAL      I95CAP = VABCAD
032400150611     C                   EVAL      I95LOC = VABLOD
032500150611     C                   CALL      'TISI95R'
032600150611     C                   PARM                    TISI95DS
032700150611     C                   EVAL      VABPRD = O95PRV
032800150611     C                   ENDIF
032900161014     C* ......VABTC1
033000161014     C                   IF        %subst(vindta:612:1) <> *blanks
033100161014     C                   EVAL      VABTC1 = 'A'
033200161014     C                   ENDIF
033300161014     C* ......VABTSP
033400161014     C                   IF        %subst(vindta:546:1) = '1'
033500161014     C                   EVAL      VABTSP = 'E'
033600161014     C                   ENDIF
033700161014     C* ......VABCBO
033800161014     C                   IF        %trim(%subst(vindta:16:3)) = '6'
033900161014     C                   EVAL      VABCBO = '2'
034000161014     C                   ENDIF
034100161014     C* ......VABTCR / VABTC1
034200161014     C                   SELECT
034300161014     C                   WHEN      %trim(%subst(vindta:533:1)) = 'T'
034400161014     C                   EVAL      VABTCR = *blanks
034500161014     C                   WHEN      %trim(%subst(vindta:533:1)) = 'E'
034600161014     C                   EVAL      VABTCR = 'P'
034700161014     C                   WHEN      %trim(%subst(vindta:533:1)) = 'C'
034800161014     C                   EVAL      VABTC1 = 'A'
034900161014     C                   ENDSL
035000161014     C* ......VABGC1 - 1° byte
035100161014     C                   SELECT
035200161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'LUN'
035300161014     C                   EVAL      VABGC1 = '1'
035400161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'MAR'
035500161014     C                   EVAL      VABGC1 = '2'
035600161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'MER'
035700161014     C                   EVAL      VABGC1 = '3'
035800161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'GIO'
035900161014     C                   EVAL      VABGC1 = '4'
036000161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'VEN'
036100161014     C                   EVAL      VABGC1 = '5'
036200161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'SAB'
036300161014     C                   EVAL      VABGC1 = '6'
036400161014     C                   WHEN      %trim(%subst(vindta:534:3)) = 'DOM'
036500161014     C                   EVAL      VABGC1 = '7'
036600161014     C                   ENDSL
036700161014     C* ......VABGC1 - 2° byte
036800161014     C                   SELECT
036900161014     C                   WHEN      %trim(%subst(vindta:537:1)) = '1'
037000161014     C                   EVAL      %subst(VABGC1:2:1) = 'M'
037100161014     C                   WHEN      %trim(%subst(vindta:537:1)) = '2'
037200161014     C                   EVAL      %subst(VABGC1:2:1) = 'P'
037300161014     C                   WHEN      %trim(%subst(vindta:537:1)) = '3'
037400161014     C                   EVAL      %subst(VABGC1:2:1) = *blanks
037500161014     C                   ENDSL
037600161014     C* ......VABGC2
037700161014     C                   SELECT
037800161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'LUN'
037900161014     C                   EVAL      VABGC2 = '1'
038000161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'MAR'
038100161014     C                   EVAL      VABGC2 = '2'
038200161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'MER'
038300161014     C                   EVAL      VABGC2 = '3'
038400161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'GIO'
038500161014     C                   EVAL      VABGC2 = '4'
038600161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'VEN'
038700161014     C                   EVAL      VABGC2 = '5'
038800161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'SAB'
038900161014     C                   EVAL      VABGC2 = '6'
039000161014     C                   WHEN      %trim(%subst(vindta:538:3)) = 'DOM'
039100161014     C                   EVAL      VABGC2 = '7'
039200161014     C                   ENDSL
039300161014     C* ......VABGC2 - 2° byte
039400161014     C                   SELECT
039500161014     C                   WHEN      %trim(%subst(vindta:541:1)) = '1'
039600161014     C                   EVAL      %subst(VABGC2:2:1) = 'M'
039700161014     C                   WHEN      %trim(%subst(vindta:541:1)) = '2'
039800161014     C                   EVAL      %subst(VABGC2:2:1) = 'P'
039900161014     C                   WHEN      %trim(%subst(vindta:541:1)) = '3'
040000161014     C                   EVAL      %subst(VABGC2:2:1) = *blanks
040100161014     C                   ENDSL
040200161014     C* ......VABNOT
040300161014     C                   EVAL      VABNOT=%trim(%subst(vindta:550:30))
040400161014     C* ......VABNT2
040500161014     C                   EVAL      VABNT2=%trim(%subst(vindta:580:30))
040600160609     C* ......VABRMN
040700161014     C                   EVAL      PiStr=%trim(%subst(vindta:7:7))
040800160617     C                   EXSR      CHKNUM
040900160617     C                   IF        PiInt=*on
041000160617     C                   Z-ADD     PiVal         VABRMN
041100160617     C                   ELSE
041200160617     C                   Z-ADD     1             VABRMN
041300160617     C                   EVAL      vinmsg = %trimr(vinmsg)
041400160617     C                             + ' ' + 'VABRMN'
041500160617     C                   ENDIF
041600161014     C* ......VATNOT_A
041700161014     C                   EVAL      VATNOT=%trim(%subst(vindta:781:20))
041800161014     C                   IF        VATNOT <> *blanks
041900161014     C                   EVAL      VATCMR = VABCMR
042000161014     C                   EVAL      VATCNT = VABCNT
042100161014     C                   EVAL      VATTRC = 'A'
042200161014     C                   ADD       1             §CTROKVT
042300161014     C                   WRITE     EDIVAT00
042400161014     C                   ENDIF
042500150611     C* ......VATNOT_B
042600161014     C                   EVAL      VATNOT=%trim(%subst(vindta:726:15))
042700161014     C                   IF        VATNOT <> *blanks
042800161014     C                   EVAL      VATCMR = VABCMR
042900161014     C                   EVAL      VATCNT = VABCNT
043000161014     C                   EVAL      VATTRC = 'B'
043100161014     C                   ADD       1             §CTROKVT
043200161014     C                   WRITE     EDIVAT00
043300161014     C                   ENDIF
043400160321     C* ......VATNOT_S
043500160509     C***                EVAL      VATNOT=%trim(%subst(vindta:509:15))
043600160509     C***                IF        VATNOT <> *blanks
043700160509     C***                EVAL      VATCMR = VABCMR
043800160509     C***                EVAL      VATCNT = VABCNT
043900160509     C***                EVAL      VATTRC = 'S'
044000160509     C***                ADD       1             §CTROKVT
044100160509     C***                WRITE     EDIVAT00
044200160509     C***                ENDIF
044300160321     C* ......VATNOT_IJ
044400161014     C                   MOVEL     *blanks       wEMAIL           70
044500161014     C                   EVAL      wEMAIL = %trim(%subst(vindta:741:40))
044600161014     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
044700161014     C                   IF        VATNOT <> *blanks
044800161014     C                   EVAL      VATCMR = VABCMR
044900161014     C                   EVAL      VATCNT = VABCNT
045000161014     C                   EVAL      VATTRC = 'I'
045100161014     C                   ADD       1             §CTROKVT
045200161014     C                   WRITE     EDIVAT00
045300161014     C                   ENDIF
045400161014     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
045500161014     C                   IF        VATNOT <> *blanks
045600161014     C                   EVAL      VATCMR = VABCMR
045700161014     C                   EVAL      VATCNT = VABCNT
045800161014     C                   EVAL      VATTRC = 'J'
045900161014     C                   ADD       1             §CTROKVT
046000161014     C                   WRITE     EDIVAT00
046100161014     C                   ENDIF
046200140922     C* ......VATNOT_P
046300150611     C***                if        *in53
046400140922     C*
046500150611     C* Solo se valorizzato RMA
046600150611     C***                if        wRMA_1 <> *blanks
046700150611     C***                eval      pIn_MaskPDF =
046800150611     C***                                  %trim(%subst(vindta:1292:100))
046900150611     C***                call      'TIS7P2SER'
047000150611     C***                parm      'W'           pIn_Opz           1
047100150611     C***                parm                    tivlrds
047200150611     C***                parm                    EDIVABDS
047300150611     C***                parm      '10'          pIn_CdIdAz        2
047400150611     C***                parm                    pIn_MaskPDF      50
047500150611     C***                parm      *blanks       pOut_Esito        1
047600150611     C***                endif
047700150611     C***                endif
047800140117     C*
047900161014     C* ......VABPKB
048000161014     C                   EVAL      PiStr=%trim(%subst(vindta:378:8))
048100160617     C                   EXSR      CHKNUM
048200160617     C                   IF        PiNum=*on
048300161003     C                   EVAL      PiVal = PiVal/1000                           * da grammi a Kg
048400161014     C                   Z-ADD     PiVal         VABPKB
048500160617     C                   ELSE
048600161014     C                   Z-ADD     *zeros        VABPKB
048700160617     C                   EVAL      vinmsg = %trimr(vinmsg)
048800160617     C                             + ' ' + 'VABPKB'
048900160617     C                   ENDIF
049000160617     C* ......VABNCL
049100161014     C                   EVAL      PiStr=%trim(%subst(vindta:373:5))
049200161014     C                   EXSR      CHKNUM
049300161014     C                   IF        PiInt=*on
049400161014     C                   Z-ADD     PiVal         VABNCL
049500161014     C                   ELSE
049600161014     C                   Z-ADD     *zeros        VABNCL
049700161014     C                   EVAL      vinmsg = %trimr(vinmsg)
049800161014     C                             + ' ' + 'VABNCL'
049900161014     C                   ENDIF
050000161014     C* ......VABDCR
050100161014     C                   IF        %trim(%subst(vindta:517:8)) <> *zeros  AND
050200161014     C                             %trim(%subst(vindta:517:8)) <> *blanks
050300161014     C                   EVAL      PiStr=%trim(%subst(vindta:517:8))
050400161014     C                   EXSR      CHKNUM
050500161014     C                   IF        PiInt=*on
050600161014     C                   Z-ADD     PiVal         VABDCR
050700161014     C                   ELSE
050800161014     C                   Z-ADD     *zeros        VABDCR
050900161014     C                   EVAL      vinmsg = %trimr(vinmsg)
051000161014     C                             + ' ' + 'VABDCR'
051100161014     C                   ENDIF
051200161014     C                   ENDIF
051300161014     C* ......VABHCR
051400161014     C                   IF        %trim(%subst(vindta:542:4)) <> *zeros  AND
051500161014     C                             %trim(%subst(vindta:542:4)) <> *blanks
051600161014     C                   EVAL      PiStr=%trim(%subst(vindta:542:4))
051700161014     C                   EXSR      CHKNUM
051800161014     C                   IF        PiInt=*on
051900161014     C                   Z-ADD     PiVal         VABHCR
052000161014     C                   ELSE
052100161014     C                   Z-ADD     *zeros        VABHCR
052200161014     C                   EVAL      vinmsg = %trimr(vinmsg)
052300161014     C                             + ' ' + 'VABHCR'
052400161014     C                   ENDIF
052500161014     C                   ENDIF
052600161014     C* ......VABCAS
052700161014     C                   IF        %trim(%subst(vindta:413:13)) <> *zeros  AND
052800161014     C                             %trim(%subst(vindta:413:13)) <> *blanks
052900161014     C                   EVAL      VABVCA=%trim(%subst(vindta:426:3))
053000161014     C                   EVAL      PiStr=%trim(%subst(vindta:413:13))
053100161014     C                   EVAL      FlgCAS = '1'
053200161014     C                   EXSR      CHKNUM
053300161014     C                   IF        PiNum=*on
053400161014     C                   EVAL      PiVal = PiVal/10000                          * gestisco 4 dec.
053500161014     C                   Z-ADD     PiVal         VABCAS
053600161014     C                   ELSE
053700161014     C                   Z-ADD     *zeros        VABCAS
053800161014     C                   EVAL      vinmsg = %trimr(vinmsg)
053900161014     C                             + ' ' + 'VABCAS'
054000161014     C                   ENDIF
054100161014     C                   ENDIF
054200161014     C* ......VABVMD
054300161014     C                   IF        %trim(%subst(vindta:486:13)) <> *zeros  AND
054400161014     C                             %trim(%subst(vindta:486:13)) <> *blanks
054500161014     C                   EVAL      VABVAD=%trim(%subst(vindta:499:3))
054600161014     C                   EVAL      PiStr=%trim(%subst(vindta:486:13))
054700161014     C                   EXSR      CHKNUM
054800161014     C                   IF        PiNum=*on
054900161014     C                   EVAL      PiVal = PiVal/10000                          * gestisco 4 dec.
055000161014     C                   Z-ADD     PiVal         VABVMD
055100161014     C                   ELSE
055200161014     C                   Z-ADD     *zeros        VABVMD
055300161014     C                   EVAL      vinmsg = %trimr(vinmsg)
055400161014     C                             + ' ' + 'VABVMD'
055500161014     C                   ENDIF
055600161014     C                   ENDIF
055700161014     C* ......VATNOT_E => sviluppo i "chi sono" DAL/AL
055800161014     C                   Z-ADD     1             wConta            5 0
055900161014     C                   EVAL      PiStr=%trim(%subst(vindta:394:7))
056000161014     C                   EXSR      CHKNUM
056100161014     C                   IF        PiInt=*on
056200161014     C                   Z-ADD     PiVal         wCollo            7 0
056300161014     C                   DOW       wConta <= VABNCL AND wConta < 999
056400161014     C                   EVAL      VATNOT=%editc(wCollo:'X')
056500151119     C                   IF        VATNOT <> *blanks
056600151119     C                   EVAL      VATCMR = VABCMR
056700151119     C                   EVAL      VATCNT = VABCNT
056800151119     C                   EVAL      VATTRC = 'E'
056900151119     C                   ADD       1             §CTROKVT
057000151119     C                   WRITE     EDIVAT00
057100151119     C                   ENDIF
057200161014     C                   ADD       1             wConta
057300161014     C                   EVAL      wCollo = wCollo + 1
057400161014     C                   ENDDO
057500151119     C                   ENDIF
057600140117     C***
057700010202     C*
057800000801     C* Ebbene...
057900000801     C                   ADD       1             §CTRMO
058000010201     C                   IF        errore <> *zeros
058100000801     C                   ADD       1             §CTRNO
058200000801     C                   EVAL      vinflg = '2'
058300000801     C                   ELSE
058400010201     C                   ADD       1             §CTROKVB
058500000801     C                   ENDIF
058600000801     C*
058700000801     C                   ENDSR
058800010202     C*----------------------------------------------------*
058900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
059000010202     C*----------------------------------------------------*
059100020305     C     PREVAT        BEGSR
059200010202     C*
059300140117     C* Compongo il nome del membro da dare al EDIVATWR
059400010202     C                   eval      parmbr = vlrhdl
059500010202     C                   movel     'M'           parmbr
059600060113     C                   eval      parccm = vlrksc
059700010202     C                   eval      paropz = '1'
059800010202     C* Effettuo la chiamata al CLLE preposto
059900140117     C                   call(e)   'TITVEVTC'
060000010202     C                   parm                    parccm
060100010202     C                   parm                    parmbr
060200010202     C                   parm                    paropz
060300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060400010202     C                   if        %error
060500010202     C                   movel     '1'           chkcall
060600010202     C                   else
060700010202     C                   movel     '0'           chkcall
060800010202     C                   endif
060900010202     C*
061000010202     C                   ENDSR
061100000801     C*----------------------------------------------------*
061200000801     C*  CONTROLLO NUMERICITA' CAMPI
061300000801     C*----------------------------------------------------*
061400000801     C     CHKNUM        BEGSR
061500000801     C*
061600000801     C                   call(e)   'ISNUMERIC'
061700000801     C                   PARM                    PiStr            30
061800160617     C                   PARM      '.'           PiDecChr          1
061900000801     C                   PARM      *ZEROS        PiVal            30 9
062000000801     C                   PARM      '0'           PiInt             1
062100000801     C                   PARM      '0'           PiNum             1
062200000801     C                   IF        %error
062300000801     C                   EVAL      PiInt=*off
062400000801     C                   ENDIF
062500000801     C*
062600000801     C                   ENDSR
062700000801     C***
062800000801
062900011113
063000011113     C*----------------------------------------------------*
063100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063200011113     C*----------------------------------------------------*
063300011113     C     CHKIMPDIV     BEGSR
063400011113     C*
063500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063600011113     C                   Z-ADD     *zeros        wrkDec            9 9
063700011113     C*
063800011113     C* Come prima cosa effettuo considerazioni sulla divisa
063900011113     C                   IF        vabIAS > *zeros
064000011113     C                   IF        vabVAS <> 'EUR'
064100011113     C                   EVAL      vabVAS =  'ITL'
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   IF        vabCAS > *zeros
064600011113     C                   IF        vabVCA <> 'EUR'
064700011113     C                   EVAL      vabVCA =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C                   IF        vabVMD > *zeros
065200020305     C                   IF        vabVAD <> 'EUR'
065300011113     C                   EVAL      vabVAD =  'ITL'
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065800011113     C                   Z-ADD     vabIAS        wrkDec
065900011113     C                   IF        wrkDec > *zeros
066000011113     C                   IF        vabVAS = 'ITL'
066100011113     C                   EVAL      vabIAS = *zeros
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066600011113     C                   Z-ADD     vabCAS        wrkDec
066700011113     C                   IF        wrkDec > *zeros
066800011113     C                   IF        vabVCA = 'ITL'
066900011113     C                   EVAL      vabCAS = *zeros
067000011113     C                   ENDIF
067100011113     C                   ENDIF
067200011113     C*
067300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
067400011113     C                   Z-ADD     vabVMD        wrkDec
067500011113     C                   IF        wrkDec > *zeros
067600011113     C                   IF        vabVAD = 'ITL'
067700011113     C                   EVAL      vabVMD = *zeros
067800011113     C                   ENDIF
067900011113     C                   ENDIF
068000011113     C*
068100011113     C                   ENDSR
068200011113     C***
068300011113
068400011113
068500000801
068600000801
068700990920      /TITLE Invio dei dati al punto operativo.
068800010202     C     invio         BEGSR
068900990920     C*
069000140117     C* 1° invio EDIVAT
069100010201     C                   reset                   dscmz
069200010201     C                   move      vlrpoi        cmzdst
069300140117     C                   eval      cmzfld = 'EDIVATWR'
069400010201     C                   eval      cmzmbd = vlrhdl
069500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600021009     C***                if        prmfir = *blanks
069700140117     C                   eval      cmzfla = 'EDIVAT0F'
069800140117     C                   eval      cmzmba = 'EDIVAT0F'
069900021009     C***                else
070000021009     C***                eval      cmzfla = prmfir
070100021009     C***                eval      cmzmba = prmfir
070200021009     C***                endif
070300010201     C                   eval      cmznrr = *zeros
070400020305     C                   move      §ctrokvt      cmznrr
070500021018     C                   eval      cmzlba = vlrfl1
070600010201     C                   call(e)   'TIS711C'
070700010201     C                   parm                    dscmz
070800010201     C                   parm      *blanks       esito
070900010205     C                   if        %error
071000010205     C                             or cmzerr = '1'
071100010205     C                             or esito  = '1'
071200010205     C                   eval      wrkesito = '3'
071300010205     C                   else
071400010201     C*
071500140117     C* 2° invio EDIVAB
071600010201     C                   reset                   dscmz
071700010201     C                   move      vlrpoi        cmzdst
071800010201     C                   eval      cmzfld = vlrfou
071900010201     C                   eval      cmzmbd = vlrhdl
072000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072100021009     C***                if        prmfir = *blanks
072200140117     C                   eval      cmzfla = 'EDIVAB0F'
072300140117     C                   eval      cmzmba = 'EDIVAB0F'
072400021009     C***                else
072500021009     C***                eval      cmzfla = prmfir
072600021009     C***                eval      cmzmba = prmfir
072700021009     C***                endif
072800010201     C                   eval      cmznrr = *zeros
072900010201     C                   move      §ctrokvb      cmznrr
073000021018     C                   eval      cmzlba = vlrfl1
073100010201     C                   call(e)   'TIS711C'
073200010201     C                   parm                    dscmz
073300010201     C                   parm      *blanks       esito
073400010201     C                   if        %error
073500010201     C                             or cmzerr = '1'
073600010201     C                             or esito  = '1'
073700010201     C                   eval      wrkesito = '3'
073800010201     C                   endif
073900010205     C                   endif
074000990920     C*
074100000613     C                   ENDSR
074200000613     C***
074300070411
074400070411     C     *pssr         BEGSR
074500070411     C*
074600070411     C                   if        %open(tivin00r)
074700070411     C                   close     tivin00r
074800070411     C                   endif
074900140117     C                   if        %open(edivabwr)
075000140117     C                   close     edivabwr
075100070411     C                   endif
075200140117     C                   if        %open(edivatwr)
075300140117     C                   close     edivatwr
075400070411     C                   endif
075500070411     C*
075600070411     C* Effettuo la chiamata al CLLE preposto
075700140117     C                   call(e)   'TITVEVTC'
075800070411     C                   parm                    parccm
075900070411     C                   parm                    parmbr
076000070411     C                   parm      '2'           paropz
076100070411     C*
076200070411     C                   eval      wrkesito = '2'
076300070411     C*
076400070411     C                   seton                                        LR
076500070411     C*
076600070411     C                   ENDSR     '*CANCL'
076700070411     C***
076800070411
076900990910
077000000613     C     *inzsr        BEGSR
077100990910     C*
077200990910     C     *entry        plist
077300990920     C                   parm                    tivlrds
077400990921     C                   parm      wrkesito      esito
077500000724     C                   parm                    prmlit
077600000710     C                   parm                    prmfir
077700000613     C*
077800000830     C* CALCOLA LA DATA CORRENTE
077900140117     C                   time                    wn14             14 0
078000140117     C                   movel     wn14          oracor            6 0          *ORA
078100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078200100722     C                   eval      datcor = %dec(%date() : *ISO)
078300000830     C*
078400000613     C                   ENDSR
078500000613     C***
