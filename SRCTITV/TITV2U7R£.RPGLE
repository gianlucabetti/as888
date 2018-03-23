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
008500100729     C*
008600100729     C                   z-add     1             wGiro             1 0
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
009700130708     C*
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
011600040802     C*
011700010305     C                   endif
011800000905     C*
011900000905     C                   else
012000000905     C                   eval      vinflg = '1'
012100000905     C                   endif
012200000905     C*
012300000905     C  N70              update    tivin000
012400000905     C*
012500991022     C  N70              ENDdo
012600100722     C*
012700100722     C* Scarico i buffer testata ancora "in canna"
012800140922     C*
012900140922     C  N31              exsr      CHKWRI
013000140117     C  N31              WRITE     EDIVAB00
013100130708     C                   SETOFF                                       31
013200140117     C*
013300140902     C                   if        *in53
013400140117     C                   call      'TIS7P2SER'
013500140117     C                   parm      'C'           pIn_Opz           1
013600140117     C                   parm                    tivlrds
013700140117     C                   parm                    EDIVABDS
013800140117     C                   parm      *blanks       pIn_CdIdAz        2
013900140117     C                   parm      *blanks       pIn_MaskPDF      50
014000140117     C                   parm      *blanks       pOut_Esito        1
014100140902     C                   endif
014200010202     C*
014300010202     C                   endif
014400990910
014500990910     C* Se non ci sono record con errori ...
014600000710     C                   if        §ctrno = 0
014700990910     C* ... restituisco esito OK.
014800990921     C                   eval      wrkesito = '0'
014900990910     C                   else
015000010201     C                   if        §ctrokvb > 0
015100990921     C                   eval      wrkesito = '1'
015200000710     C                   else
015300000710     C                   eval      wrkesito = '2'
015400990910     C                   endif
015500000710     C                   endif
015600990910     C*
015700990914     C                   if        %open(tivin00r)
015800990908     C                   close     tivin00r
015900990914     C                   endif
016000140117     C                   if        %open(edivabwr)
016100140117     C                   close     edivabwr
016200990914     C                   endif
016300140117     C                   if        %open(edivatwr)
016400140117     C                   close     edivatwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   if        §ctrokvb > 0
016800000724     C                             and vlrpoi <> *zeros
016900010202     C                   exsr      invio
017000990920     C                   endif
017100990920     C*
017200910830     C                   ENDSR
017300000613     C***
017400140922
017500140922     C*----------------------------------------------------*
017600140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017700140922     C*----------------------------------------------------*
017800140922     C     CHKWRI        BEGSR
017900140922     C*
018000140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018100140922     C                   IF        FlgCAS <> '0'
018200140922     C                   IF        VABCBO = '1'
018300140922     C                   EVAL      VABCBO = '4'
018400140922     C                   ELSE
018500140922     C                   EVAL      VABCBO = '6'
018600140922     C                   ENDIF
018700140922     C                   ENDIF
018800140922     C*
018900140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019000140922     C                   EXSR      CHKIMPDIV
019100140922     C*
019200140922     C                   ENDSR
019300990920
019400000801     C*----------------------------------------------------*
019500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019600000801     C*----------------------------------------------------*
019700010201     C     INZVAR        BEGSR
019800000801     C*
019900040802     C                   Z-ADD     *zeros        Num5_0            5 0
020000040802     C                   MOVEL     '0'           FlgCAS            1
020100000801     C*
020200000801     C                   ENDSR
020300000801     C*----------------------------------------------------*
020400040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020500000801     C*----------------------------------------------------*
020600000801     C     DEFCAM        BEGSR
020700000801     C*
020800140117     C                   CLEAR                   EDIVAB00
020900140117     C                   CLEAR                   EDIVAT00
021000020619     C* Imposto i valori di default...
021100160509     C                   Z-ADD     2493699       VABCCM
021200160509     C                   Z-ADD     2493699       VATCCM
021300160509     C                   Z-ADD     249           VABLNP
021400160509     C                   Z-ADD     249           VATLNP
021500160509     C                   Z-ADD     000           VABCTR
021600151119     C                   MOVEL     '7Q'          VABCTM
021700040823     C                   MOVEL     '1'           VABCBO
021800020619     C* ... e poi verifico se sono stati passati come parametri
021900020619     C                   IF        vlrppt > *blanks
022000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022200020619     C                   EXSR      CHKNUM
022300020619     C                   IF        PiInt=*on
022400020619     C                   Z-ADD     PiVal         VABCCM
022500020619     C                   Z-ADD     PiVal         VATCCM
022600020619     C                   ENDIF
022700040506     C                   ENDIF
022800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023000020619     C                   EXSR      CHKNUM
023100020619     C                   IF        PiInt=*on
023200020619     C                   Z-ADD     PiVal         VABLNP
023300020619     C                   Z-ADD     PiVal         VATLNP
023400040506     C                   ENDIF
023500020619     C                   ENDIF
023600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023800020619     C                   EXSR      CHKNUM
023900020619     C                   IF        PiInt=*on
024000020619     C                   Z-ADD     PiVal         VABCTR
024100040506     C                   ENDIF
024200020619     C                   ENDIF
024300060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024400060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024500060202     C                   ENDIF
024600140902     C                   IF        %subst(vlrppt:16:1) = 'S'
024700140902     C                   SETON                                        53
024800140902     C                   ELSE
024900140902     C                   SETOFF                                       53
025000140902     C                   ENDIF
025100020619     C                   ENDIF
025200000801     C*
025300000801     C                   ENDSR
025400000801     C*----------------------------------------------------*
025500140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025600000801     C*----------------------------------------------------*
025700040910     C     IMPVABVAT     BEGSR
025800040910     C*
025900040910     C* Traduzione relativa ai tipi record del file del cliente
026000040910     C*
026100071210     C*
026200071210     C***
026300151119     C                   MOVEL     *blanks       savCMR           35
026400160509     C                   EVAL      savCMR = 'EURACCORDI '+%trim(%char(%date()))
026500140922     C***
026600160509     C* ...tipo record 'BO' (testata)
026700160509     C                   IF        %trim(%subst(vindta:1:2)) = 'BO'
026800100722     C*
026900100722     C* Se nn primo giro => scarico il buffer precedente
027000150611     C                   if        wGiro = 1
027100100722     C                   eval      wGiro = 2
027200100722     C                   else
027300140922     C*
027400140922     C  N31              exsr      CHKWRI
027500140117     C  N31              WRITE     EDIVAB00
027600130708     C                   SETOFF                                       31
027700100722     C                   endif
027800100722     C*
027900071210     C* Resetto indicatore d anomalia sul singolo record
028000071210     C                   eval      vinflg = '1'
028100071210     C* ......inizializzazioni iniziali e formati record file Bartolini
028200071210     C                   EXSR      INZVAR
028300071210     C                   EXSR      DEFCAM
028400071210     C*
028500071210     C                   Z-ADD     *zeros        errore            1 0
028600071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
028700071210     C                   MOVEL     datcor        VABAAS
028800071210     C                   MOVEL     datcor        VATAAS
028900071210     C                   MOVE      datcor        VABMGS
029000071210     C                   MOVE(P)   vlrpoi        VABFGS
029100071210     C                   MOVE(P)   vlrpoi        VATFGS
029200140903     C*
029300140903     C* Dai "fissi"
029400140903     C                   EVAL      VABCMR = savCMR
029500140903     C                   EVAL      VABDCM = datcor
029600140903     C                   EVAL      VABDTS = datcor
029700140903     C                   EVAL      VABHMS = oracor
029800140903     C                   EVAL      VABCNT = 1
029900140903     C*
030000071210     C* ......VABNSP/VATNSP
030100071210     C* NSP => Stacco un numeratore da AZNUM
030200160609     C                   clear                   TRUL33DS
030300160609     C                   eval      I33OPE = *zeros
030400160609     C                   eval      I33CNU = 302
030500160609     C                   eval      I33NUM = 1
030600160609     C                   movel     TRUL33DS      KPJBU
030700160609     C                   call      'TRUL33R'
030800160609     C                   parm                    KPJBA
030900160609     C                   movel     KPJBU         TRUL33DS
031000160609     C                   if        O33ERR = *zeros
031100160609     C                   z-add     O33NRF        VABNSP
031200160609     C                   z-add     O33NRF        VATNSP
031300160609     C                   else
031400160609     C                   Z-ADD     1             errore
031500160609     C                   EVAL      vinmsg = %trimr(vinmsg)
031600160609     C                             + ' ' + 'VABNSP VATNSP'
031700160609     C                   endif
031800151119     C* ......VABRMA
031900160609     C***                EVAL      VABRMA=%trim(%subst(vindta:48:20))
032000160609     C                   EVAL      VABRMA=%trim(%subst(vindta:14:20))
032100150611     C* ......VABRSD
032200160509     C                   EVAL      VABRSD=%trim(%subst(vindta:83:35))
032300151119     C* ......VABRD2
032400160509     C                   EVAL      VABRD2=%trim(%subst(vindta:118:35))
032500150611     C* ......VABIND
032600160610     C                   EVAL      VABIND=%trim(%subst(vindta:153:35))
032700160610     C* ......VABCAD
032800160610     C                   EVAL      VABCAD=%trim(%subst(vindta:223:7))
032900150611     C* ......VABLOD
033000160509     C                   EVAL      VABLOD=%trim(%subst(vindta:188:35))
033100150611     C* ......VABPRD
033200160509     C                   EVAL      VABPRD=%trim(%subst(vindta:230:2))
033300150611     C* ......VABPRD
033400150611     C* Reperisco la provincia dal CAP e dalla località
033500150611     C                   IF        VABPRD  = *blanks AND
033600150611     C                             VABLOD <> *blanks AND
033700150611     C                             VABCAD <> *blanks AND
033800150611     C                             VABNZD  = *blanks
033900150611     C                   CLEAR                   TISI95DS
034000150611     C                   EVAL      I95TCN = '3'
034100150611     C                   Z-ADD     datcor        I95DAT
034200150611     C                   EVAL      I95CAP = VABCAD
034300150611     C                   EVAL      I95LOC = VABLOD
034400150611     C                   CALL      'TISI95R'
034500150611     C                   PARM                    TISI95DS
034600150611     C                   EVAL      VABPRD = O95PRV
034700150611     C                   ENDIF
034800160215     C* ......VABNOT / VABNT2
034900160509     C                   EVAL      VABNOT = %trim(%subst(vindta:324:35))
035000160509     C                   EVAL      VABNT2 = %trim(%subst(vindta:324+35:60-35))
035100160609     C* ......VABRMN
035200160609     C                   EVAL      VABRMN = VABNSP
035300160609     C***                EVAL      PiStr=%trim(%subst(vindta:14:20))
035400160609     C***                EXSR      CHKNUM
035500160609     C***                IF        PiInt=*on
035600160609     C***                Z-ADD     PiVal         VABNSP
035700160609     C***                Z-ADD     PiVal         VABRMN
035800160609     C***                ELSE
035900160609     C***                ADD       1             errore
036000160609     C***                EVAL      vinmsg = %trimr(vinmsg)
036100160609     C***                          + ' ' + 'VABNSP VABRMN'
036200160609     C***                ENDIF
036300151119     C* ......VABPKB
036400160509     C                   EVAL      PiStr=%trim(%subst(vindta:255:8))
036500151119     C                   EXSR      CHKNUM
036600151119     C                   IF        PiNum=*on
036700160509     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
036800151119     C                   Z-ADD     PiVal         VABPKB
036900151119     C                   ELSE
037000151119     C                   ADD       1             errore
037100151119     C                   EVAL      vinmsg = %trimr(vinmsg)
037200151119     C                             + ' ' + 'VABPKB'
037300151119     C                   ENDIF
037400160509     C* ......VABVLB
037500160509     C                   EVAL      PiStr=%trim(%subst(vindta:263:7))
037600160509     C                   EXSR      CHKNUM
037700160509     C                   IF        PiNum=*on
037800160509     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
037900160509     C                   Z-ADD     PiVal         VABVLB
038000160509     C                   ELSE
038100160509     C                   ADD       1             errore
038200160509     C                   EVAL      vinmsg = %trimr(vinmsg)
038300160509     C                             + ' ' + 'VABVLB'
038400160509     C                   ENDIF
038500151119     C* ......VABNCL
038600160509     C                   EVAL      PiStr=%trim(%subst(vindta:270:5))
038700151119     C                   EXSR      CHKNUM
038800151119     C                   IF        PiInt=*on
038900151119     C                   ADD       PiVal         VABNCL
039000151119     C                   ELSE
039100151119     C                   ADD       1             errore
039200151119     C                   EVAL      vinmsg = %trimr(vinmsg)
039300151119     C                             + ' ' + 'VABNCL'
039400151119     C                   ENDIF
039500160509     C* ......VABCAS
039600160509     C                   IF        %trim(%subst(vindta:275:15)) <> *zeros AND
039700160509     C                             %trim(%subst(vindta:275:15)) <> *blanks
039800160509     C                   EVAL      FlgCAS = '1'
039900160509     C                   EVAL      VABVCA = 'EUR'
040000160509     C                   EVAL      PiStr=%trim(%subst(vindta:275:15))
040100160509     C                   EXSR      CHKNUM
040200160509     C                   IF        PiNum=*on
040300160509     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
040400160509     C                   Z-ADD     PiVal         VABCAS
040500160509     C                   ELSE
040600160509     C                   ADD       1             errore
040700160509     C                   EVAL      vinmsg = %trimr(vinmsg)
040800160509     C                             + ' ' + 'VABCAS'
040900160509     C                   ENDIF
041000160509     C                   ENDIF
041100160509     C* ......VABIAS
041200160509     C                   IF        %trim(%subst(vindta:309:15)) <> *zeros AND
041300160509     C                             %trim(%subst(vindta:309:15)) <> *blanks
041400160509     C                   EVAL      VABVAS = 'EUR'
041500160509     C                   EVAL      PiStr=%trim(%subst(vindta:309:15))
041600160509     C                   EXSR      CHKNUM
041700160509     C                   IF        PiNum=*on
041800160509     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
041900160509     C                   Z-ADD     PiVal         VABIAS
042000160509     C                   ELSE
042100160509     C                   ADD       1             errore
042200160509     C                   EVAL      vinmsg = %trimr(vinmsg)
042300160509     C                             + ' ' + 'VABIAS'
042400160509     C                   ENDIF
042500160509     C                   ENDIF
042600160509     C* ......VABCBO
042700160509     C                   IF        %trim(%subst(vindta:384:4)) = 'EPA'
042800160509     C                   EVAL      VABCBO = '2'
042900160509     C                   ENDIF
043000150611     C* ......VATNOT_B
043100160509     C***                EVAL      VATNOT=%trim(%subst(vindta:509:15))
043200160509     C***                IF        VATNOT <> *blanks
043300160509     C***                EVAL      VATCMR = VABCMR
043400160509     C***                EVAL      VATCNT = VABCNT
043500160509     C***                EVAL      VATTRC = 'B'
043600160509     C***                ADD       1             §CTROKVT
043700160509     C***                WRITE     EDIVAT00
043800160509     C***                ENDIF
043900160321     C* ......VATNOT_S
044000160509     C***                EVAL      VATNOT=%trim(%subst(vindta:509:15))
044100160509     C***                IF        VATNOT <> *blanks
044200160509     C***                EVAL      VATCMR = VABCMR
044300160509     C***                EVAL      VATCNT = VABCNT
044400160509     C***                EVAL      VATTRC = 'S'
044500160509     C***                ADD       1             §CTROKVT
044600160509     C***                WRITE     EDIVAT00
044700160509     C***                ENDIF
044800160321     C* ......VATNOT_IJ
044900160509     C***                IF        %trim(%subst(vindta:509:15)) = *blanks
045000160509     C***                EVAL      VATNOT=%trim(%subst(vindta:597:35))
045100160509     C***                IF        VATNOT <> *blanks
045200160509     C***                EVAL      VATCMR = VABCMR
045300160509     C***                EVAL      VATCNT = VABCNT
045400160509     C***                EVAL      VATTRC = 'I'
045500160509     C***                ADD       1             §CTROKVT
045600160509     C***                WRITE     EDIVAT00
045700160509     C***                ENDIF
045800160509     C***                EVAL      VATNOT=%trim(%subst(vindta:597+35:40-35))
045900160509     C***                IF        VATNOT <> *blanks
046000160509     C***                EVAL      VATCMR = VABCMR
046100160509     C***                EVAL      VATCNT = VABCNT
046200160509     C***                EVAL      VATTRC = 'J'
046300160509     C***                ADD       1             §CTROKVT
046400160509     C***                WRITE     EDIVAT00
046500160509     C***                ENDIF
046600160509     C***                ENDIF
046700140922     C* ......VATNOT_P
046800150611     C***                if        *in53
046900140922     C*
047000150611     C* Solo se valorizzato RMA
047100150611     C***                if        wRMA_1 <> *blanks
047200150611     C***                eval      pIn_MaskPDF =
047300150611     C***                                  %trim(%subst(vindta:1292:100))
047400150611     C***                call      'TIS7P2SER'
047500150611     C***                parm      'W'           pIn_Opz           1
047600150611     C***                parm                    tivlrds
047700150611     C***                parm                    EDIVABDS
047800150611     C***                parm      '10'          pIn_CdIdAz        2
047900150611     C***                parm                    pIn_MaskPDF      50
048000150611     C***                parm      *blanks       pOut_Esito        1
048100150611     C***                endif
048200150611     C***                endif
048300140117     C*
048400140117     C                   ENDIF
048500151119     C***
048600160509     C* ...tipo record 'CO' (dettagli segnacollo)
048700160509     C                   IF        %trim(%subst(vindta:1:2)) = 'CO'
048800160509     C* ......VATNSP
048900160610     C***                EVAL      PiStr=%trim(%subst(vindta:3:15))
049000160610     C***                EXSR      CHKNUM
049100160610     C***                IF        PiInt=*on
049200160610     C***                Z-ADD     PiVal         VATNSP
049300160610     C***                ELSE
049400160610     C***                ADD       1             errore
049500160610     C***                EVAL      vinmsg = %trimr(vinmsg)
049600160610     C***                          + ' ' + 'VATNSP'
049700160610     C***                ENDIF
049800151119     C* ......VATNOT_E
049900160509     C                   EVAL      VATNOT=%trim(%subst(vindta:18:30))
050000151119     C                   IF        VATNOT <> *blanks
050100151119     C                   EVAL      VATCMR = VABCMR
050200151119     C                   EVAL      VATCNT = VABCNT
050300151119     C                   EVAL      VATTRC = 'E'
050400151119     C                   ADD       1             §CTROKVT
050500151119     C                   WRITE     EDIVAT00
050600151119     C                   ENDIF
050700151119     C*
050800151119     C                   ENDIF
050900140117     C***
051000010202     C*
051100000801     C* Ebbene...
051200000801     C                   ADD       1             §CTRMO
051300010201     C                   IF        errore <> *zeros
051400000801     C                   ADD       1             §CTRNO
051500000801     C                   EVAL      vinflg = '2'
051600000801     C                   ELSE
051700010201     C                   ADD       1             §CTROKVB
051800000801     C                   ENDIF
051900000801     C*
052000000801     C                   ENDSR
052100010202     C*----------------------------------------------------*
052200140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
052300010202     C*----------------------------------------------------*
052400020305     C     PREVAT        BEGSR
052500010202     C*
052600140117     C* Compongo il nome del membro da dare al EDIVATWR
052700010202     C                   eval      parmbr = vlrhdl
052800010202     C                   movel     'M'           parmbr
052900060113     C                   eval      parccm = vlrksc
053000010202     C                   eval      paropz = '1'
053100010202     C* Effettuo la chiamata al CLLE preposto
053200140117     C                   call(e)   'TITVEVTC'
053300010202     C                   parm                    parccm
053400010202     C                   parm                    parmbr
053500010202     C                   parm                    paropz
053600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053700010202     C                   if        %error
053800010202     C                   movel     '1'           chkcall
053900010202     C                   else
054000010202     C                   movel     '0'           chkcall
054100010202     C                   endif
054200010202     C*
054300010202     C                   ENDSR
054400000801     C*----------------------------------------------------*
054500000801     C*  CONTROLLO NUMERICITA' CAMPI
054600000801     C*----------------------------------------------------*
054700000801     C     CHKNUM        BEGSR
054800000801     C*
054900000801     C                   call(e)   'ISNUMERIC'
055000000801     C                   PARM                    PiStr            30
055100040714     C                   PARM      ','           PiDecChr          1
055200000801     C                   PARM      *ZEROS        PiVal            30 9
055300000801     C                   PARM      '0'           PiInt             1
055400000801     C                   PARM      '0'           PiNum             1
055500000801     C                   IF        %error
055600000801     C                   EVAL      PiInt=*off
055700000801     C                   ENDIF
055800000801     C*
055900000801     C                   ENDSR
056000000801     C***
056100000801
056200011113
056300011113     C*----------------------------------------------------*
056400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056500011113     C*----------------------------------------------------*
056600011113     C     CHKIMPDIV     BEGSR
056700011113     C*
056800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056900011113     C                   Z-ADD     *zeros        wrkDec            9 9
057000011113     C*
057100011113     C* Come prima cosa effettuo considerazioni sulla divisa
057200011113     C                   IF        vabIAS > *zeros
057300011113     C                   IF        vabVAS <> 'EUR'
057400011113     C                   EVAL      vabVAS =  'ITL'
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C                   IF        vabCAS > *zeros
057900011113     C                   IF        vabVCA <> 'EUR'
058000011113     C                   EVAL      vabVCA =  'ITL'
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C                   IF        vabVMD > *zeros
058500020305     C                   IF        vabVAD <> 'EUR'
058600011113     C                   EVAL      vabVAD =  'ITL'
058700011113     C                   ENDIF
058800011113     C                   ENDIF
058900011113     C*
059000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059100011113     C                   Z-ADD     vabIAS        wrkDec
059200011113     C                   IF        wrkDec > *zeros
059300011113     C                   IF        vabVAS = 'ITL'
059400011113     C                   EVAL      vabIAS = *zeros
059500011113     C                   ENDIF
059600011113     C                   ENDIF
059700011113     C*
059800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059900011113     C                   Z-ADD     vabCAS        wrkDec
060000011113     C                   IF        wrkDec > *zeros
060100011113     C                   IF        vabVCA = 'ITL'
060200011113     C                   EVAL      vabCAS = *zeros
060300011113     C                   ENDIF
060400011113     C                   ENDIF
060500011113     C*
060600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060700011113     C                   Z-ADD     vabVMD        wrkDec
060800011113     C                   IF        wrkDec > *zeros
060900011113     C                   IF        vabVAD = 'ITL'
061000011113     C                   EVAL      vabVMD = *zeros
061100011113     C                   ENDIF
061200011113     C                   ENDIF
061300011113     C*
061400011113     C                   ENDSR
061500011113     C***
061600011113
061700011113
061800000801
061900000801
062000990920      /TITLE Invio dei dati al punto operativo.
062100010202     C     invio         BEGSR
062200990920     C*
062300140117     C* 1° invio EDIVAT
062400010201     C                   reset                   dscmz
062500010201     C                   move      vlrpoi        cmzdst
062600140117     C                   eval      cmzfld = 'EDIVATWR'
062700010201     C                   eval      cmzmbd = vlrhdl
062800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062900021009     C***                if        prmfir = *blanks
063000140117     C                   eval      cmzfla = 'EDIVAT0F'
063100140117     C                   eval      cmzmba = 'EDIVAT0F'
063200021009     C***                else
063300021009     C***                eval      cmzfla = prmfir
063400021009     C***                eval      cmzmba = prmfir
063500021009     C***                endif
063600010201     C                   eval      cmznrr = *zeros
063700020305     C                   move      §ctrokvt      cmznrr
063800021018     C                   eval      cmzlba = vlrfl1
063900010201     C                   call(e)   'TIS711C'
064000010201     C                   parm                    dscmz
064100010201     C                   parm      *blanks       esito
064200010205     C                   if        %error
064300010205     C                             or cmzerr = '1'
064400010205     C                             or esito  = '1'
064500010205     C                   eval      wrkesito = '3'
064600010205     C                   else
064700010201     C*
064800140117     C* 2° invio EDIVAB
064900010201     C                   reset                   dscmz
065000010201     C                   move      vlrpoi        cmzdst
065100010201     C                   eval      cmzfld = vlrfou
065200010201     C                   eval      cmzmbd = vlrhdl
065300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065400021009     C***                if        prmfir = *blanks
065500140117     C                   eval      cmzfla = 'EDIVAB0F'
065600140117     C                   eval      cmzmba = 'EDIVAB0F'
065700021009     C***                else
065800021009     C***                eval      cmzfla = prmfir
065900021009     C***                eval      cmzmba = prmfir
066000021009     C***                endif
066100010201     C                   eval      cmznrr = *zeros
066200010201     C                   move      §ctrokvb      cmznrr
066300021018     C                   eval      cmzlba = vlrfl1
066400010201     C                   call(e)   'TIS711C'
066500010201     C                   parm                    dscmz
066600010201     C                   parm      *blanks       esito
066700010201     C                   if        %error
066800010201     C                             or cmzerr = '1'
066900010201     C                             or esito  = '1'
067000010201     C                   eval      wrkesito = '3'
067100010201     C                   endif
067200010205     C                   endif
067300990920     C*
067400000613     C                   ENDSR
067500000613     C***
067600070411
067700070411     C     *pssr         BEGSR
067800070411     C*
067900070411     C                   if        %open(tivin00r)
068000070411     C                   close     tivin00r
068100070411     C                   endif
068200140117     C                   if        %open(edivabwr)
068300140117     C                   close     edivabwr
068400070411     C                   endif
068500140117     C                   if        %open(edivatwr)
068600140117     C                   close     edivatwr
068700070411     C                   endif
068800070411     C*
068900070411     C* Effettuo la chiamata al CLLE preposto
069000140117     C                   call(e)   'TITVEVTC'
069100070411     C                   parm                    parccm
069200070411     C                   parm                    parmbr
069300070411     C                   parm      '2'           paropz
069400070411     C*
069500070411     C                   eval      wrkesito = '2'
069600070411     C*
069700070411     C                   seton                                        LR
069800070411     C*
069900070411     C                   ENDSR     '*CANCL'
070000070411     C***
070100070411
070200990910
070300000613     C     *inzsr        BEGSR
070400990910     C*
070500990910     C     *entry        plist
070600990920     C                   parm                    tivlrds
070700990921     C                   parm      wrkesito      esito
070800000724     C                   parm                    prmlit
070900000710     C                   parm                    prmfir
071000000613     C*
071100000830     C* CALCOLA LA DATA CORRENTE
071200140117     C                   time                    wn14             14 0
071300140117     C                   movel     wn14          oracor            6 0          *ORA
071400100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071500100722     C                   eval      datcor = %dec(%date() : *ISO)
071600000830     C*
071700000613     C                   ENDSR
071800000613     C***
