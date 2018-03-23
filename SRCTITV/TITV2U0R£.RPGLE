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
002600140922     D depspe          s             17    INZ(*blanks)
002700000830
002800041025     D*------------------
002900041025     D* DS REPERIMENTO NUMERATORE
003000041025     D*------------------
003100041025     D trul33ds      e ds                  inz
003200041025     D*------------------
003300041025     D* DS ARCHITETTURA
003400041025     D*------------------
003500041025     D kpjba         e ds                  inz
003600041025     D*------------------
003700990908
003800010201
003900010201
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300000613     C*
004400150213     C                   EXSR      DEFCAM
004500150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004600070719     C*
004700070719     C* Esegue lancio TISI95R solo x chiusura
004800070719     C                   CLEAR                   TISI95DS
004900070719     C                   EVAL      I95TLA = 'C'
005000070719     C                   CALL      'TISI95R'
005100070719     C                   PARM                    TISI95DS
005200000613     C*
005300010202     C* Effettuo la chiamata al CLLE preposto
005400140922     C                   call(e)   'TITVEVTC'
005500140922     C                   parm                    parccm
005600140922     C                   parm                    parmbr
005700140922     C                   parm      '2'           paropz
005800000616     C*
005900010201     C                   seton                                        LR
006000990908
006100000801
006200910830     C*--------------------------------------------------------
006300140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006400910830     C*--------------------------------------------------------
006500040526     C     RWFILE        BEGSR
006600990910     C*
006700990914     C                   if        not %open(tivin00r)
006800990908     C                   open      tivin00r
006900990914     C                   endif
007000140117     C                   if        not %open(edivabwr)
007100140117     C                   open      edivabwr
007200990914     C                   endif
007300140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007400020305     C                   exsr      prevat
007500010201     C*
007600010202     C                   if        chkcall = '0'
007700010202     C*
007800140117     C                   if        not %open(edivatwr)
007900140117     C                   open      edivatwr
008000010201     C                   endif
008100990910     C*
008200010201     C                   clear                   §CTROKVB          5 0
008300020305     C                   clear                   §CTROKVT          5 0
008400000801     C                   clear                   §CTRMO            5 0
008500000801     C                   clear                   §CTRNO            5 0
008600100729     C*
008700100729     C                   z-add     1             wGiro             1 0
008800140117     C*
008900140902     C                   if        *in53
009000140117     C                   call      'TIS7P2SER'
009100140117     C                   parm      'O'           pIn_Opz           1
009200140117     C                   parm                    tivlrds
009300140117     C                   parm                    EDIVABDS
009400140117     C                   parm      *blanks       pIn_CdIdAz        2
009500140117     C                   parm      *blanks       pIn_MaskPDF      50
009600140117     C                   parm      *blanks       pOut_Esito        1
009700140902     C                   endif
009800130708     C*
009900130708     C                   SETOFF                                       31
010000921023     C                   DO        *HIVAL
010100990913     C*
010200990915     C                   READ      tivin00r                               70
010300040910     C                   if        vindta > *blanks
010400000613     C                   add       1             rrnum
010500000801     C*
010600000801     C                   if        *in70 = *off
010700000801     C                             and
010800000801     C                             (vinflg = *blanks
010900000801     C                              or vinflg = '0'
011000000801     C                              or vinflg = '2')
011100000801     C*
011200000801     C                   clear                   vinmsg
011300000801     C                   eval      vinflg = '1'
011400040910     C*
011500040910     C* Eseguo routine d traduzione
011600040910     C                   exsr      impvabvat
011700040802     C*
011800010305     C                   endif
011900000905     C*
012000000905     C                   else
012100000905     C                   eval      vinflg = '1'
012200000905     C                   endif
012300000905     C*
012400000905     C  N70              update    tivin000
012500000905     C*
012600991022     C  N70              ENDdo
012700100722     C*
012800100722     C* Scarico i buffer testata ancora "in canna"
012900140922     C*
013000140922     C  N31              exsr      CHKWRI
013100140117     C  N31              WRITE     EDIVAB00
013200130708     C                   SETOFF                                       31
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
018200140922     C                   IF        FlgCAS <> '0'
018300140922     C                   IF        VABCBO = '1'
018400140922     C                   EVAL      VABCBO = '4'
018500140922     C                   ELSE
018600140922     C                   EVAL      VABCBO = '6'
018700140922     C                   ENDIF
018800140922     C                   ENDIF
018900140922     C*
019000140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019100140922     C                   EXSR      CHKIMPDIV
019200140922     C*
019300140922     C                   ENDSR
019400990920
019500000801     C*----------------------------------------------------*
019600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019700000801     C*----------------------------------------------------*
019800010201     C     INZVAR        BEGSR
019900000801     C*
020000040802     C                   Z-ADD     *zeros        Num5_0            5 0
020100040802     C                   MOVEL     '0'           FlgCAS            1
020200000801     C*
020300000801     C                   ENDSR
020400000801     C*----------------------------------------------------*
020500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020600000801     C*----------------------------------------------------*
020700000801     C     DEFCAM        BEGSR
020800000801     C*
020900140117     C                   CLEAR                   EDIVAB00
021000140117     C                   CLEAR                   EDIVAT00
021100020619     C* Imposto i valori di default...
021200150611     C                   Z-ADD     1320019       VABCCM
021300150611     C                   Z-ADD     1320019       VATCCM
021400150611     C                   Z-ADD     132           VABLNP
021500150611     C                   Z-ADD     132           VATLNP
021600150611     C                   Z-ADD     000           VABCTR
021700150611     C                   MOVEL     *blanks       VABCTM
021800040823     C                   MOVEL     '1'           VABCBO
021900020619     C* ... e poi verifico se sono stati passati come parametri
022000020619     C                   IF        vlrppt > *blanks
022100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022300020619     C                   EXSR      CHKNUM
022400020619     C                   IF        PiInt=*on
022500020619     C                   Z-ADD     PiVal         VABCCM
022600020619     C                   Z-ADD     PiVal         VATCCM
022700020619     C                   ENDIF
022800040506     C                   ENDIF
022900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023100020619     C                   EXSR      CHKNUM
023200020619     C                   IF        PiInt=*on
023300020619     C                   Z-ADD     PiVal         VABLNP
023400020619     C                   Z-ADD     PiVal         VATLNP
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023900020619     C                   EXSR      CHKNUM
024000020619     C                   IF        PiInt=*on
024100020619     C                   Z-ADD     PiVal         VABCTR
024200040506     C                   ENDIF
024300020619     C                   ENDIF
024400060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024500060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024600060202     C                   ENDIF
024700140902     C                   IF        %subst(vlrppt:16:1) = 'S'
024800140902     C                   SETON                                        53
024900140902     C                   ELSE
025000140902     C                   SETOFF                                       53
025100140902     C                   ENDIF
025200020619     C                   ENDIF
025300000801     C*
025400000801     C                   ENDSR
025500000801     C*----------------------------------------------------*
025600140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025700000801     C*----------------------------------------------------*
025800040910     C     IMPVABVAT     BEGSR
025900040910     C*
026000040910     C* Traduzione relativa ai tipi record del file del cliente
026100040910     C*
026200071210     C*
026300071210     C***
026400150611     C* ...tipo record 'EDI_DC40' (info CMR)
026500150611     C                   IF        %trim(%subst(vindta:1:8)) = 'EDI_DC40'
026600140903     C                   MOVEL     *blanks       savCMR           35
026700150611     C                   EVAL      savCMR = %trim(%subst(vindta:14:16)) + ' '+
026800140922     C                                      %char(datcor) + ' ' + %char(oracor)
026900130620     C                   ENDIF
027000140922     C***
027100150611     C* ...tipo record 'E2EDL20003' (rottuta di codice spedizione)
027200150611     C                   IF        %trim(%subst(vindta:1:10)) = 'E2EDL20003'
027300100722     C*
027400100722     C* Se nn primo giro => scarico il buffer precedente
027500150611     C                   if        wGiro = 1
027600100722     C                   eval      wGiro = 2
027700100722     C                   else
027800140922     C*
027900140922     C  N31              exsr      CHKWRI
028000140117     C  N31              WRITE     EDIVAB00
028100130708     C                   SETOFF                                       31
028200100722     C                   endif
028300100722     C*
028400071210     C* Resetto indicatore d anomalia sul singolo record
028500071210     C                   eval      vinflg = '1'
028600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
028700071210     C                   EXSR      INZVAR
028800071210     C                   EXSR      DEFCAM
028900071210     C*
029000071210     C                   Z-ADD     *zeros        errore            1 0
029100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
029200071210     C                   MOVEL     datcor        VABAAS
029300071210     C                   MOVEL     datcor        VATAAS
029400071210     C                   MOVE      datcor        VABMGS
029500071210     C                   MOVE(P)   vlrpoi        VABFGS
029600071210     C                   MOVE(P)   vlrpoi        VATFGS
029700140903     C*
029800140903     C* Dai "fissi"
029900140903     C                   EVAL      VABCMR = savCMR
030000140903     C                   EVAL      VABDCM = datcor
030100140903     C                   EVAL      VABDTS = datcor
030200140903     C                   EVAL      VABHMS = oracor
030300140903     C                   EVAL      VABCNT = 1
030400140903     C*
030500071210     C* ......VABNSP/VATNSP
030600071210     C* NSP => Stacco un numeratore da AZNUM
030700071210     C                   clear                   TRUL33DS
030800071210     C                   eval      I33OPE = *zeros
030900071210     C                   eval      I33CNU = 302
031000071210     C                   eval      I33NUM = 1
031100071210     C                   movel     TRUL33DS      KPJBU
031200071210     C                   call      'TRUL33R'
031300071210     C                   parm                    KPJBA
031400071210     C                   movel     KPJBU         TRUL33DS
031500071210     C                   if        O33ERR = *zeros
031600071210     C                   z-add     O33NRF        VABNSP
031700071210     C                   z-add     O33NRF        VATNSP
031800071210     C                   else
031900071210     C                   Z-ADD     1             errore
032000071210     C                   EVAL      vinmsg = %trimr(vinmsg)
032100071210     C                             + ' ' + 'VABNSP VATNSP'
032200071210     C                   endif
032300140922     C*
032400140922     C                   ENDIF
032500140922     C***
032600150611     C* ...tipo record 'E2EDL20003' (info testata - spedizione)
032700150611     C                   IF        %trim(%subst(vindta:1:10)) = 'E2EDL20003'
032800150611     C* ......VABRMN
032900150611     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
033000150611     C                   EXSR      CHKNUM
033100150611     C                   IF        PiInt=*on
033200150611     C                   Z-ADD     PiVal         VABRMN
033300150611     C                   ELSE
033400150611     C                   ADD       1             errore
033500150611     C                   EVAL      vinmsg = %trimr(vinmsg)
033600150611     C                             + ' ' + 'VABRMN'
033700150611     C                   ENDIF
033800150611     C* ......VABPKB
033900150611     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
034000150611     C                   EXSR      CHKNUM
034100150611     C                   IF        PiNum=*on
034200150611     C                   EVAL      PiVal = PiVAl / 1000
034300150611     C                   ADD       PiVal         VABPKB
034400150611     C                   ELSE
034500150611     C                   ADD       1             errore
034600150611     C                   EVAL      vinmsg = %trimr(vinmsg)
034700150611     C                             + ' ' + 'VABPKB'
034800150611     C                   ENDIF
034900150611     C* ......VABVLB
035000150611     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
035100150611     C                   EXSR      CHKNUM
035200150611     C                   IF        PiNum=*on
035300150611     C                   EVAL      PiVal = PiVAl / 1000
035400150611     C                   ADD       PiVal         VABVLB
035500150611     C                   ELSE
035600150611     C                   ADD       1             errore
035700150611     C                   EVAL      vinmsg = %trimr(vinmsg)
035800150611     C                             + ' ' + 'VABVLB'
035900150611     C                   ENDIF
036000150611     C* ......VABNCL
036100150611     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
036200150611     C                   EXSR      CHKNUM
036300150611     C                   IF        PiInt=*on
036400150611     C                   ADD       PiVal         VABNCL
036500150611     C                   ELSE
036600150611     C                   ADD       1             errore
036700150611     C                   EVAL      vinmsg = %trimr(vinmsg)
036800150611     C                             + ' ' + 'VABNCL'
036900150611     C                   ENDIF
037000150611     C*
037100150611     C                   ENDIF
037200150611     C***
037300150611     C* ...tipo record 'E2ADRM1001' (info testata - destinatario)
037400150611     C                   IF        %trim(%subst(vindta:1:10)) = 'E2ADRM1001' and
037500150611     C                             %trim(%subst(vindta:64:3)) = 'WE'
037600150611     C                   MOVEL     *blanks       wDEST            70
037700150611     C                   EVAL      wDEST = %trim(%subst(vindta:119:80))
037800150611     C* ......VABRSD
037900150611     C                   EVAL      VABRSD=%subst(wDEST:1:35)
038000150611     C* ......VABRD2
038100150611     C                   EVAL      VABRD2=%subst(wDEST:36:35)
038200150611     C* ......VABIND
038300150611     C                   EVAL      VABIND=%trim(%subst(vindta:439:40))
038400150611     C* ......VABCAD
038500150611     C                   EVAL      VABCAD=%trim(%subst(vindta:573:10))
038600150611     C* ......VABLOD
038700150611     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40))
038800150611     C* ......VABPRD
038900150611     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
039000150611     C* ......VABNZD
039100150611     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
039200150611     C                   IF        VABNZD='IT'
039300150611     C                   EVAL      VABNZD=*blanks
039400150611     C                   ENDIF
039500150611     C* ......VABPRD
039600150611     C* Reperisco la provincia dal CAP e dalla località
039700150611     C                   IF        VABPRD  = *blanks AND
039800150611     C                             VABLOD <> *blanks AND
039900150611     C                             VABCAD <> *blanks AND
040000150611     C                             VABNZD  = *blanks
040100150611     C                   CLEAR                   TISI95DS
040200150611     C                   EVAL      I95TCN = '3'
040300150611     C                   Z-ADD     datcor        I95DAT
040400150611     C                   EVAL      I95CAP = VABCAD
040500150611     C                   EVAL      I95LOC = VABLOD
040600150611     C                   CALL      'TISI95R'
040700150611     C                   PARM                    TISI95DS
040800150611     C                   EVAL      VABPRD = O95PRV
040900150611     C                   ENDIF
041000150611     C* ......VATNOT_B
041100150611     C                   EVAL      VATNOT=%trim(%subst(vindta:748:30))
041200150611     C                   IF        VATNOT <> *blanks
041300150611     C                   EVAL      VATCMR = VABCMR
041400150611     C                   EVAL      VATCNT = VABCNT
041500150611     C                   EVAL      VATTRC = 'B'
041600150611     C                   ADD       1             §CTROKVT
041700150611     C                   WRITE     EDIVAT00
041800150611     C                   ENDIF
041900150611     C* ......VATNOT_I_J
042000150611     C                   MOVEL     *blanks       wEMAIL           70
042100150611     C                   EVAL      wEMAIL = %trim(%subst(vindta:868:70))
042200150921     C* siccome devo togliere la scrittura dei tracciati I e J da VAT, finto che mi arrivi blank
042300150921     C                   MOVEL     *blanks       wEMAIL
042400150611     C* ......VATNOT_I
042500150611     C                   EVAL      VATNOT=%subst(wEMAIL:1:35)
042600150611     C                   IF        VATNOT <> *blanks
042700150611     C                   EVAL      VATCMR = VABCMR
042800150611     C                   EVAL      VATCNT = VABCNT
042900150611     C                   EVAL      VATTRC = 'I'
043000150921     C                   ADD       1             §CTROKVT
043100150921     C                   WRITE     EDIVAT00
043200150611     C                   ENDIF
043300150611     C* ......VATNOT_J
043400150611     C                   EVAL      VATNOT=%subst(wEMAIL:36:35)
043500150611     C                   IF        VATNOT <> *blanks
043600150611     C                   EVAL      VATCMR = VABCMR
043700150611     C                   EVAL      VATCNT = VABCNT
043800150611     C                   EVAL      VATTRC = 'J'
043900150611     C                   ADD       1             §CTROKVT
044000150921     C                   WRITE     EDIVAT00
044100150611     C                   ENDIF
044200150611     C*
044300150611     C                   ENDIF
044400150611     C***
044500150611     C* ...tipo record 'E2EDL43000' (info testata)
044600150611     C                   IF        %trim(%subst(vindta:1:10)) = 'E2EDL43000'
044700150611     C* ......VABRMA
044800150611     C                   EVAL      VABRMA=%trim(%subst(vindta:64:36))
044900150611     C* ......VABCAS
045000150611     C***                IF        %subst(vindta:145:12) <> *all'0'
045100150611     C***                EVAL      FlgCAS = '1'
045200150611     C***                EVAL      VABVCA = 'EUR'
045300150611     C***                EVAL      PiStr=%trim(%subst(vindta:145:12))
045400150611     C***                EXSR      CHKNUM
045500150611     C***                IF        PiNum=*on
045600150611     C***                EVAL      PiVal = PiVAl / 100
045700150611     C***                ADD       PiVal         VABCAS
045800150611     C***                ELSE
045900150611     C***                ADD       1             errore
046000150611     C***                EVAL      vinmsg = %trimr(vinmsg)
046100150611     C***                          + ' ' + 'VABCAS'
046200150611     C***                ENDIF
046300150611     C***                ENDIF
046400140922     C* ......VABIAS
046500150611     C***                IF        %subst(vindta:157:12) <> *all'0'
046600150611     C***                EVAL      VABVAS = 'EUR'
046700150611     C***                EVAL      PiStr=%trim(%subst(vindta:157:12))
046800150611     C***                EXSR      CHKNUM
046900150611     C***                IF        PiNum=*on
047000150611     C***                EVAL      PiVal = PiVAl / 100
047100150611     C***                ADD       PiVal         VABIAS
047200150611     C***                ELSE
047300150611     C***                ADD       1             errore
047400150611     C***                EVAL      vinmsg = %trimr(vinmsg)
047500150611     C***                          + ' ' + 'VABIAS'
047600150611     C***                ENDIF
047700150611     C***                ENDIF
047800150611     C* ......VABDCR / VABHCR
047900150611     C***                IF        %trim(%subst(vindta:142:2)) = '2'
048000150611     C***                IF        %trim(%subst(vindta:125:8)) <> *blanks AND
048100150611     C***                          %trim(%subst(vindta:125:8)) <> *zeros
048200150611     C***                EVAL      PiStr=%trim(%subst(vindta:125:8))
048300150611     C***                EXSR      CHKNUM
048400150611     C***                IF        PiInt=*on
048500150611     C***                Z-ADD     PiVal         VABDCR
048600150611     C***                ELSE
048700150611     C***                ADD       1             errore
048800150611     C***                EVAL      vinmsg = %trimr(vinmsg)
048900150611     C***                          + ' ' + 'VABDCR'
049000150611     C***                ENDIF
049100150611     C***                EVAL      PiStr=%trim(%subst(vindta:133:4))
049200150611     C***                EXSR      CHKNUM
049300150611     C***                IF        PiInt=*on
049400150611     C***                Z-ADD     PiVal         VABHCR
049500150611     C***                ELSE
049600150611     C***                ADD       1             errore
049700150611     C***                EVAL      vinmsg = %trimr(vinmsg)
049800150611     C***                          + ' ' + 'VABHCR'
049900150611     C***                ENDIF
050000150611     C***                ENDIF
050100150611     C***                ENDIF
050200140922     C* ......VATNOT_P
050300150611     C***                if        *in53
050400140922     C*
050500150611     C* Solo se valorizzato RMA
050600150611     C***                if        wRMA_1 <> *blanks
050700150611     C***                eval      pIn_MaskPDF =
050800150611     C***                                  %trim(%subst(vindta:1292:100))
050900150611     C***                call      'TIS7P2SER'
051000150611     C***                parm      'W'           pIn_Opz           1
051100150611     C***                parm                    tivlrds
051200150611     C***                parm                    EDIVABDS
051300150611     C***                parm      '10'          pIn_CdIdAz        2
051400150611     C***                parm                    pIn_MaskPDF      50
051500150611     C***                parm      *blanks       pOut_Esito        1
051600150611     C***                endif
051700150611     C***                endif
051800140117     C*
051900140117     C                   ENDIF
052000140117     C***
052100010202     C*
052200000801     C* Ebbene...
052300000801     C                   ADD       1             §CTRMO
052400010201     C                   IF        errore <> *zeros
052500000801     C                   ADD       1             §CTRNO
052600000801     C                   EVAL      vinflg = '2'
052700000801     C                   ELSE
052800010201     C                   ADD       1             §CTROKVB
052900000801     C                   ENDIF
053000000801     C*
053100000801     C                   ENDSR
053200010202     C*----------------------------------------------------*
053300140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053400010202     C*----------------------------------------------------*
053500020305     C     PREVAT        BEGSR
053600010202     C*
053700140117     C* Compongo il nome del membro da dare al EDIVATWR
053800010202     C                   eval      parmbr = vlrhdl
053900010202     C                   movel     'M'           parmbr
054000060113     C                   eval      parccm = vlrksc
054100010202     C                   eval      paropz = '1'
054200010202     C* Effettuo la chiamata al CLLE preposto
054300140117     C                   call(e)   'TITVEVTC'
054400010202     C                   parm                    parccm
054500010202     C                   parm                    parmbr
054600010202     C                   parm                    paropz
054700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054800010202     C                   if        %error
054900010202     C                   movel     '1'           chkcall
055000010202     C                   else
055100010202     C                   movel     '0'           chkcall
055200010202     C                   endif
055300010202     C*
055400010202     C                   ENDSR
055500000801     C*----------------------------------------------------*
055600000801     C*  CONTROLLO NUMERICITA' CAMPI
055700000801     C*----------------------------------------------------*
055800000801     C     CHKNUM        BEGSR
055900000801     C*
056000000801     C                   call(e)   'ISNUMERIC'
056100000801     C                   PARM                    PiStr            30
056200040714     C                   PARM      ','           PiDecChr          1
056300000801     C                   PARM      *ZEROS        PiVal            30 9
056400000801     C                   PARM      '0'           PiInt             1
056500000801     C                   PARM      '0'           PiNum             1
056600000801     C                   IF        %error
056700000801     C                   EVAL      PiInt=*off
056800000801     C                   ENDIF
056900000801     C*
057000000801     C                   ENDSR
057100000801     C***
057200000801
057300011113
057400011113     C*----------------------------------------------------*
057500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057600011113     C*----------------------------------------------------*
057700011113     C     CHKIMPDIV     BEGSR
057800011113     C*
057900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058000011113     C                   Z-ADD     *zeros        wrkDec            9 9
058100011113     C*
058200011113     C* Come prima cosa effettuo considerazioni sulla divisa
058300011113     C                   IF        vabIAS > *zeros
058400011113     C                   IF        vabVAS <> 'EUR'
058500011113     C                   EVAL      vabVAS =  'ITL'
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   IF        vabCAS > *zeros
059000011113     C                   IF        vabVCA <> 'EUR'
059100011113     C                   EVAL      vabVCA =  'ITL'
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C                   IF        vabVMD > *zeros
059600020305     C                   IF        vabVAD <> 'EUR'
059700011113     C                   EVAL      vabVAD =  'ITL'
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060200011113     C                   Z-ADD     vabIAS        wrkDec
060300011113     C                   IF        wrkDec > *zeros
060400011113     C                   IF        vabVAS = 'ITL'
060500011113     C                   EVAL      vabIAS = *zeros
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
061000011113     C                   Z-ADD     vabCAS        wrkDec
061100011113     C                   IF        wrkDec > *zeros
061200011113     C                   IF        vabVCA = 'ITL'
061300011113     C                   EVAL      vabCAS = *zeros
061400011113     C                   ENDIF
061500011113     C                   ENDIF
061600011113     C*
061700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061800011113     C                   Z-ADD     vabVMD        wrkDec
061900011113     C                   IF        wrkDec > *zeros
062000011113     C                   IF        vabVAD = 'ITL'
062100011113     C                   EVAL      vabVMD = *zeros
062200011113     C                   ENDIF
062300011113     C                   ENDIF
062400011113     C*
062500011113     C                   ENDSR
062600011113     C***
062700011113
062800011113
062900000801
063000000801
063100990920      /TITLE Invio dei dati al punto operativo.
063200010202     C     invio         BEGSR
063300990920     C*
063400140117     C* 1° invio EDIVAT
063500010201     C                   reset                   dscmz
063600010201     C                   move      vlrpoi        cmzdst
063700140117     C                   eval      cmzfld = 'EDIVATWR'
063800010201     C                   eval      cmzmbd = vlrhdl
063900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064000021009     C***                if        prmfir = *blanks
064100140117     C                   eval      cmzfla = 'EDIVAT0F'
064200140117     C                   eval      cmzmba = 'EDIVAT0F'
064300021009     C***                else
064400021009     C***                eval      cmzfla = prmfir
064500021009     C***                eval      cmzmba = prmfir
064600021009     C***                endif
064700010201     C                   eval      cmznrr = *zeros
064800020305     C                   move      §ctrokvt      cmznrr
064900021018     C                   eval      cmzlba = vlrfl1
065000010201     C                   call(e)   'TIS711C'
065100010201     C                   parm                    dscmz
065200010201     C                   parm      *blanks       esito
065300010205     C                   if        %error
065400010205     C                             or cmzerr = '1'
065500010205     C                             or esito  = '1'
065600010205     C                   eval      wrkesito = '3'
065700010205     C                   else
065800010201     C*
065900140117     C* 2° invio EDIVAB
066000010201     C                   reset                   dscmz
066100010201     C                   move      vlrpoi        cmzdst
066200010201     C                   eval      cmzfld = vlrfou
066300010201     C                   eval      cmzmbd = vlrhdl
066400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066500021009     C***                if        prmfir = *blanks
066600140117     C                   eval      cmzfla = 'EDIVAB0F'
066700140117     C                   eval      cmzmba = 'EDIVAB0F'
066800021009     C***                else
066900021009     C***                eval      cmzfla = prmfir
067000021009     C***                eval      cmzmba = prmfir
067100021009     C***                endif
067200010201     C                   eval      cmznrr = *zeros
067300010201     C                   move      §ctrokvb      cmznrr
067400021018     C                   eval      cmzlba = vlrfl1
067500010201     C                   call(e)   'TIS711C'
067600010201     C                   parm                    dscmz
067700010201     C                   parm      *blanks       esito
067800010201     C                   if        %error
067900010201     C                             or cmzerr = '1'
068000010201     C                             or esito  = '1'
068100010201     C                   eval      wrkesito = '3'
068200010201     C                   endif
068300010205     C                   endif
068400990920     C*
068500000613     C                   ENDSR
068600000613     C***
068700070411
068800070411     C     *pssr         BEGSR
068900070411     C*
069000070411     C                   if        %open(tivin00r)
069100070411     C                   close     tivin00r
069200070411     C                   endif
069300140117     C                   if        %open(edivabwr)
069400140117     C                   close     edivabwr
069500070411     C                   endif
069600140117     C                   if        %open(edivatwr)
069700140117     C                   close     edivatwr
069800070411     C                   endif
069900070411     C*
070000070411     C* Effettuo la chiamata al CLLE preposto
070100140117     C                   call(e)   'TITVEVTC'
070200070411     C                   parm                    parccm
070300070411     C                   parm                    parmbr
070400070411     C                   parm      '2'           paropz
070500070411     C*
070600070411     C                   eval      wrkesito = '2'
070700070411     C*
070800070411     C                   seton                                        LR
070900070411     C*
071000070411     C                   ENDSR     '*CANCL'
071100070411     C***
071200070411
071300990910
071400000613     C     *inzsr        BEGSR
071500990910     C*
071600990910     C     *entry        plist
071700990920     C                   parm                    tivlrds
071800990921     C                   parm      wrkesito      esito
071900000724     C                   parm                    prmlit
072000000710     C                   parm                    prmfir
072100000613     C*
072200000830     C* CALCOLA LA DATA CORRENTE
072300140117     C                   time                    wn14             14 0
072400140117     C                   movel     wn14          oracor            6 0          *ORA
072500100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072600100722     C                   eval      datcor = %dec(%date() : *ISO)
072700000830     C*
072800000613     C                   ENDSR
072900000613     C***
