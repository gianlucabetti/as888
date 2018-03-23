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
004300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004400070719     C*
004500070719     C* Esegue lancio TISI95R solo x chiusura
004600070719     C                   CLEAR                   TISI95DS
004700070719     C                   EVAL      I95TLA = 'C'
004800070719     C                   CALL      'TISI95R'
004900070719     C                   PARM                    TISI95DS
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200140120     C***                call(e)   'TITVEVTC'
005300140120     C***                parm                    parccm
005400140120     C***                parm                    parmbr
005500140120     C***                parm      '2'           paropz
005600000616     C*
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800140117     C                   if        not %open(edivabwr)
006900140117     C                   open      edivabwr
007000990914     C                   endif
007100140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600140117     C                   if        not %open(edivatwr)
007700140117     C                   open      edivatwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400100729     C*
008500100729     C                   z-add     1             wGiro             1 0
008600140117     C*
008700140902     C                   if        *in53
008800140117     C                   call      'TIS7P2SER'
008900140117     C                   parm      'O'           pIn_Opz           1
009000140117     C                   parm                    tivlrds
009100140117     C                   parm                    EDIVABDS
009200140117     C                   parm      *blanks       pIn_CdIdAz        2
009300140117     C                   parm      *blanks       pIn_MaskPDF      50
009400140117     C                   parm      *blanks       pOut_Esito        1
009500140902     C                   endif
009600130708     C*
009700130708     C                   SETOFF                                       31
009800921023     C                   DO        *HIVAL
009900990913     C*
010000990915     C                   READ      tivin00r                               70
010100040910     C                   if        vindta > *blanks
010200000613     C                   add       1             rrnum
010300000801     C*
010400000801     C                   if        *in70 = *off
010500000801     C                             and
010600000801     C                             (vinflg = *blanks
010700000801     C                              or vinflg = '0'
010800000801     C                              or vinflg = '2')
010900000801     C*
011000000801     C                   clear                   vinmsg
011100000801     C                   eval      vinflg = '1'
011200040910     C*
011300040910     C* Eseguo routine d traduzione
011400040910     C                   exsr      impvabvat
011500040802     C*
011600010305     C                   endif
011700000905     C*
011800000905     C                   else
011900000905     C                   eval      vinflg = '1'
012000000905     C                   endif
012100000905     C*
012200000905     C  N70              update    tivin000
012300000905     C*
012400991022     C  N70              ENDdo
012500100722     C*
012600100722     C* Scarico i buffer testata ancora "in canna"
012700140117     C  N31              WRITE     EDIVAB00
012800130708     C                   SETOFF                                       31
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
017100990920
017200000801     C*----------------------------------------------------*
017300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017400000801     C*----------------------------------------------------*
017500010201     C     INZVAR        BEGSR
017600000801     C*
017700040802     C                   Z-ADD     *zeros        Num5_0            5 0
017800040802     C                   MOVEL     '0'           FlgCAS            1
017900000801     C*
018000000801     C                   ENDSR
018100000801     C*----------------------------------------------------*
018200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
018300000801     C*----------------------------------------------------*
018400000801     C     DEFCAM        BEGSR
018500000801     C*
018600140117     C                   CLEAR                   EDIVAB00
018700140117     C                   CLEAR                   EDIVAT00
018800020619     C* Imposto i valori di default...
018900140117     C                   Z-ADD     2495088       VABCCM
019000140117     C                   Z-ADD     2495088       VATCCM
019100140117     C                   Z-ADD     249           VABLNP
019200140117     C                   Z-ADD     249           VATLNP
019300070531     C                   Z-ADD     000           VABCTR
019400140117     C                   MOVEL     '7T'          VABCTM
019500040823     C                   MOVEL     '1'           VABCBO
019600020619     C* ... e poi verifico se sono stati passati come parametri
019700020619     C                   IF        vlrppt > *blanks
019800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020000020619     C                   EXSR      CHKNUM
020100020619     C                   IF        PiInt=*on
020200020619     C                   Z-ADD     PiVal         VABCCM
020300020619     C                   Z-ADD     PiVal         VATCCM
020400020619     C                   ENDIF
020500040506     C                   ENDIF
020600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020800020619     C                   EXSR      CHKNUM
020900020619     C                   IF        PiInt=*on
021000020619     C                   Z-ADD     PiVal         VABLNP
021100020619     C                   Z-ADD     PiVal         VATLNP
021200040506     C                   ENDIF
021300020619     C                   ENDIF
021400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCTR
021900040506     C                   ENDIF
022000020619     C                   ENDIF
022100060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
022200060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
022300060202     C                   ENDIF
022400140902     C                   IF        %subst(vlrppt:16:1) = 'S'
022500140902     C                   SETON                                        53
022600140902     C                   ELSE
022700140902     C                   SETOFF                                       53
022800140902     C                   ENDIF
022900020619     C                   ENDIF
023000000801     C*
023100000801     C                   ENDSR
023200000801     C*----------------------------------------------------*
023300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
023400000801     C*----------------------------------------------------*
023500040910     C     IMPVABVAT     BEGSR
023600040910     C*
023700040910     C* Traduzione relativa ai tipi record del file del cliente
023800040910     C*
023900071210     C*
024000071210     C***
024100140117     C* ...tipo record 'HEADINFO' (info CMR)
024200140117     C                   IF        %trim(%subst(vindta:1:10)) = 'HEADINFO'
024300140903     C                   MOVEL     *blanks       savCMR           35
024400140903     C                   EVAL      savCMR = %trim(%subst(vindta:45:35))
024500130620     C                   ENDIF
024600130620     C***
024700140117     C* ...tipo record 'CONSI001' (info testata)
024800140117     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI001'
024900100722     C*
025000100722     C* Se nn primo giro => scarico il buffer precedente
025100100722     C                   if        wGiro = 1
025200100722     C                   eval      wGiro = 2
025300100722     C                   else
025400140117     C  N31              WRITE     EDIVAB00
025500130708     C                   SETOFF                                       31
025600100722     C                   endif
025700100722     C*
025800071210     C* Resetto indicatore d anomalia sul singolo record
025900071210     C                   eval      vinflg = '1'
026000071210     C* ......inizializzazioni iniziali e formati record file Bartolini
026100071210     C                   EXSR      INZVAR
026200071210     C                   EXSR      DEFCAM
026300071210     C*
026400071210     C                   Z-ADD     *zeros        errore            1 0
026500071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
026600071210     C                   MOVEL     datcor        VABAAS
026700071210     C                   MOVEL     datcor        VATAAS
026800071210     C                   MOVE      datcor        VABMGS
026900071210     C                   MOVE(P)   vlrpoi        VABFGS
027000071210     C                   MOVE(P)   vlrpoi        VATFGS
027100140903     C*
027200140903     C* Dai "fissi"
027300140903     C                   EVAL      VABCMR = savCMR
027400140903     C                   EVAL      VABDCM = datcor
027500140903     C                   EVAL      VABDTS = datcor
027600140903     C                   EVAL      VABHMS = oracor
027700140903     C                   EVAL      VABCNT = 1
027800140903     C*
027900071210     C* ......VABNSP/VATNSP
028000071210     C* NSP => Stacco un numeratore da AZNUM
028100071210     C                   clear                   TRUL33DS
028200071210     C                   eval      I33OPE = *zeros
028300071210     C                   eval      I33CNU = 302
028400071210     C                   eval      I33NUM = 1
028500071210     C                   movel     TRUL33DS      KPJBU
028600071210     C                   call      'TRUL33R'
028700071210     C                   parm                    KPJBA
028800071210     C                   movel     KPJBU         TRUL33DS
028900071210     C                   if        O33ERR = *zeros
029000071210     C                   z-add     O33NRF        VABNSP
029100071210     C                   z-add     O33NRF        VATNSP
029200071210     C                   else
029300071210     C                   Z-ADD     1             errore
029400071210     C                   EVAL      vinmsg = %trimr(vinmsg)
029500071210     C                             + ' ' + 'VABNSP VATNSP'
029600071210     C                   endif
029700140117     C* ......VABRMA
029800140117     C                   EVAL      VABRMA=%trim(%subst(vindta:15:25))
029900140117     C* ......VABDCR
030000140117     C                   EVAL      PiStr=%trim(%subst(vindta:40:8))
030100140117     C                   EXSR      CHKNUM
030200140117     C                   IF        PiInt=*on
030300140910     C                   Z-ADD     PiVal         VABDCR
030400140117     C                   ELSE
030500140117     C                   ADD       1             errore
030600140117     C                   EVAL      vinmsg = %trimr(vinmsg)
030700140910     C                             + ' ' + 'VABDCR'
030800140117     C                   ENDIF
030900140117     C* ......VABCAS
031000140117     C                   IF        %subst(vindta:56:15) <> *all'0'
031100140117     C                   EVAL      FlgCAS = '1'
031200140117     C                   EVAL      PiStr=%trim(%subst(vindta:56:15))
031300140117     C                   EXSR      CHKNUM
031400140117     C                   IF        PiNum=*on
031500140117     C                   EVAL      VABCAS = PiVal / 100
031600140117     C                   ELSE
031700140117     C                   ADD       1             errore
031800140117     C                   EVAL      vinmsg = %trimr(vinmsg)
031900140117     C                             + ' ' + 'VABCAS'
032000140117     C                   ENDIF
032100140117     C                   ENDIF
032200140117     C*
032300140117     C* Considerazioni sul contenuto di campi precedentemente valorizzati
032400140117     C                   IF        FlgCAS <> '0'
032500140117     C                   IF        VABCBO = '1'
032600140117     C                   EVAL      VABCBO = '4'
032700140117     C                   ELSE
032800140117     C                   EVAL      VABCBO = '6'
032900140117     C                   ENDIF
033000140117     C                   ENDIF
033100140117     C*
033200140117     C* Eseguo routine finale x considerazioni specifiche su importi/divise
033300140117     C                   EXSR      CHKIMPDIV
033400140117     C*
033500140117     C                   ENDIF
033600140117     C***
033700140117     C* ...tipo record 'CONSI004.1' (info testata)
033800140117     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI004.1'
033900140117     C* ......VABNOT
034000140117     C                   EVAL      VABNOT=%trim(%subst(vindta:69:25))
034100140117     C* ......VABRMN
034200140910     C                   EVAL      PiStr=%trim(%subst(vindta:69:6))
034300140117     C                   EXSR      CHKNUM
034400140117     C                   IF        PiInt=*on
034500140117     C                   Z-ADD     PiVal         VABRMN
034600140117     C                   ELSE
034700140117     C                   ADD       1             errore
034800140117     C                   EVAL      vinmsg = %trimr(vinmsg)
034900140117     C                             + ' ' + 'VABRMN'
035000140117     C                   ENDIF
035100140117     C* ......VATNOT_P
035200140902     C                   if        *in53
035300140117     C*
035400140121     C* Solo se valorizzato RMN
035500140121     C                   if        VABRMN > *zeros
035600140117     C                   eval      pIn_MaskPDF = '&0RMN*.pdf'
035700140117     C                   call      'TIS7P2SER'
035800140117     C                   parm      'W'           pIn_Opz           1
035900140117     C                   parm                    tivlrds
036000140117     C                   parm                    EDIVABDS
036100140117     C                   parm      '10'          pIn_CdIdAz        2
036200140117     C                   parm                    pIn_MaskPDF      50
036300140117     C                   parm      *blanks       pOut_Esito        1
036400140121     C                   endif
036500140902     C                   endif
036600140117     C* ......VABNCL
036700140117     C                   EVAL      PiStr=%trim(%subst(vindta:96:4))
036800140117     C                   EXSR      CHKNUM
036900140117     C                   IF        PiInt=*on
037000140117     C                   Z-ADD     PiVal         VABNCL
037100140117     C                   ELSE
037200140117     C                   ADD       1             errore
037300140117     C                   EVAL      vinmsg = %trimr(vinmsg)
037400140117     C                             + ' ' + 'VABNCL'
037500140117     C                   ENDIF
037600140117     C* ......VABPKB
037700140117     C                   EVAL      PiStr=%trim(%subst(vindta:100:7))
037800140117     C                   EXSR      CHKNUM
037900140117     C                   IF        PiNum=*on
038000140117     C                   ADD       PiVal         VABPKB
038100140117     C                   ELSE
038200140117     C                   ADD       1             errore
038300140117     C                   EVAL      vinmsg = %trimr(vinmsg)
038400140117     C                             + ' ' + 'VABPKB'
038500140117     C                   ENDIF
038600140117     C*
038700140117     C                   ENDIF
038800140117     C***
038900140117     C* ...tipo record 'CONSI005' (info testata)
039000140117     C                   IF        %trim(%subst(vindta:1:10)) = 'CONSI005'
039100140117     C* Solo sub-types 'CN' o 'DP'
039200140117     C                   IF        %trim(%subst(vindta:11:3)) = 'CN' OR
039300140117     C                             %trim(%subst(vindta:11:3)) = 'DP'
039400060202     C* ......VABRSD
039500140117     C                   EVAL      VABRSD=%trim(%subst(vindta:14:35))
039600130620     C* ......VABRD2
039700140117     C                   EVAL      VABRD2=%trim(%subst(vindta:84:35))
039800100714     C* ......VABIND
039900140117     C                   EVAL      VABIND=%trim(%subst(vindta:49:35))
040000130620     C* ......VABCAD
040100140117     C                   EVAL      VABCAD=%trim(%subst(vindta:154:9))
040200060202     C* ......VABLOD
040300140117     C                   EVAL      VABLOD=%trim(%subst(vindta:119:35))
040400130620     C* ......VABNZD
040500140117     C                   EVAL      VABNZD=%trim(%subst(vindta:163:3))
040600130620     C                   IF        VABNZD='IT'
040700130620     C                   EVAL      VABNZD=*blanks
040800130620     C                   ENDIF
040900140918     C* ......VABPRD
041000140918     C                   EVAL      VABPRD=*blanks
041100060202     C* ......VABPRD
041200070719     C* Reperisco la provincia dal CAP e dalla località
041300070719     C                   IF        VABPRD  = *blanks AND
041400130620     C                             VABLOD <> *blanks AND
041500130620     C                             VABCAD <> *blanks AND
041600130620     C                             VABNZD  = *blanks
041700070719     C                   CLEAR                   TISI95DS
041800070719     C                   EVAL      I95TCN = '3'
041900070719     C                   Z-ADD     datcor        I95DAT
042000070719     C                   EVAL      I95CAP = VABCAD
042100070719     C                   EVAL      I95LOC = VABLOD
042200070719     C                   CALL      'TISI95R'
042300070719     C                   PARM                    TISI95DS
042400070719     C                   EVAL      VABPRD = O95PRV
042500070719     C                   ENDIF
042600140117     C*
042700140117     C                   ENDIF
042800140117     C*
042900140117     C                   ENDIF
043000140117     C***
043100140117     C* ...tipo record 'COLLI001' (info dettagli)
043200140117     C                   IF        %trim(%subst(vindta:1:10)) = 'COLLI001'
043300110425     C* Resetto indicatore d anomalia sul singolo record
043400110425     C                   eval      vinflg = '1'
043500130620     C* ......VATNOT_E
043600140909     C                   EVAL      VATNOT = %trim(%subst(vindta:69:35))
043700140909     C***                EVAL      PiStr=%trim(%subst(vindta:69:35))
043800140909     C***                EXSR      CHKNUM
043900140909     C***                IF        PiInt=*on
044000140909     C***                Z-ADD     PiVal         wNUMNOT_E        35 0
044100140909     C***                EVAL      VATNOT=%trim(%editc(wNUMNOT_E:'4'))
044200140117     C* x ogni tipo record 'COLLI001' devo scaricare il buffer del file EDIVAT
044300100722     C                   IF        VATNOT <> *blanks
044400140117     C                   EVAL      VATCMR = VABCMR
044500140117     C                   EVAL      VATCNT = VABCNT
044600140117     C                   EVAL      VATTRC = 'E'
044700100722     C                   ADD       1             §CTROKVT
044800140117     C                   WRITE     EDIVAT00
044900100722     C                   ENDIF
045000140909     C***                ENDIF
045100100722     C*
045200100722     C                   ENDIF
045300010202     C*
045400000801     C* Ebbene...
045500000801     C                   ADD       1             §CTRMO
045600010201     C                   IF        errore <> *zeros
045700000801     C                   ADD       1             §CTRNO
045800000801     C                   EVAL      vinflg = '2'
045900000801     C                   ELSE
046000010201     C                   ADD       1             §CTROKVB
046100000801     C                   ENDIF
046200000801     C*
046300000801     C                   ENDSR
046400010202     C*----------------------------------------------------*
046500140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
046600010202     C*----------------------------------------------------*
046700020305     C     PREVAT        BEGSR
046800010202     C*
046900140117     C* Compongo il nome del membro da dare al EDIVATWR
047000010202     C                   eval      parmbr = vlrhdl
047100010202     C                   movel     'M'           parmbr
047200060113     C                   eval      parccm = vlrksc
047300010202     C                   eval      paropz = '1'
047400010202     C* Effettuo la chiamata al CLLE preposto
047500140117     C                   call(e)   'TITVEVTC'
047600010202     C                   parm                    parccm
047700010202     C                   parm                    parmbr
047800010202     C                   parm                    paropz
047900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000010202     C                   if        %error
048100010202     C                   movel     '1'           chkcall
048200010202     C                   else
048300010202     C                   movel     '0'           chkcall
048400010202     C                   endif
048500010202     C*
048600010202     C                   ENDSR
048700000801     C*----------------------------------------------------*
048800000801     C*  CONTROLLO NUMERICITA' CAMPI
048900000801     C*----------------------------------------------------*
049000000801     C     CHKNUM        BEGSR
049100000801     C*
049200000801     C                   call(e)   'ISNUMERIC'
049300000801     C                   PARM                    PiStr            30
049400040714     C                   PARM      ','           PiDecChr          1
049500000801     C                   PARM      *ZEROS        PiVal            30 9
049600000801     C                   PARM      '0'           PiInt             1
049700000801     C                   PARM      '0'           PiNum             1
049800000801     C                   IF        %error
049900000801     C                   EVAL      PiInt=*off
050000000801     C                   ENDIF
050100000801     C*
050200000801     C                   ENDSR
050300000801     C***
050400000801
050500011113
050600011113     C*----------------------------------------------------*
050700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050800011113     C*----------------------------------------------------*
050900011113     C     CHKIMPDIV     BEGSR
051000011113     C*
051100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051200011113     C                   Z-ADD     *zeros        wrkDec            9 9
051300011113     C*
051400011113     C* Come prima cosa effettuo considerazioni sulla divisa
051500011113     C                   IF        vabIAS > *zeros
051600011113     C                   IF        vabVAS <> 'EUR'
051700011113     C                   EVAL      vabVAS =  'ITL'
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C                   IF        vabCAS > *zeros
052200011113     C                   IF        vabVCA <> 'EUR'
052300011113     C                   EVAL      vabVCA =  'ITL'
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C                   IF        vabVMD > *zeros
052800020305     C                   IF        vabVAD <> 'EUR'
052900011113     C                   EVAL      vabVAD =  'ITL'
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053400011113     C                   Z-ADD     vabIAS        wrkDec
053500011113     C                   IF        wrkDec > *zeros
053600011113     C                   IF        vabVAS = 'ITL'
053700011113     C                   EVAL      vabIAS = *zeros
053800011113     C                   ENDIF
053900011113     C                   ENDIF
054000011113     C*
054100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
054200011113     C                   Z-ADD     vabCAS        wrkDec
054300011113     C                   IF        wrkDec > *zeros
054400011113     C                   IF        vabVCA = 'ITL'
054500011113     C                   EVAL      vabCAS = *zeros
054600011113     C                   ENDIF
054700011113     C                   ENDIF
054800011113     C*
054900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055000011113     C                   Z-ADD     vabVMD        wrkDec
055100011113     C                   IF        wrkDec > *zeros
055200011113     C                   IF        vabVAD = 'ITL'
055300011113     C                   EVAL      vabVMD = *zeros
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C                   ENDSR
055800011113     C***
055900011113
056000011113
056100000801
056200000801
056300990920      /TITLE Invio dei dati al punto operativo.
056400010202     C     invio         BEGSR
056500990920     C*
056600140117     C* 1° invio EDIVAT
056700010201     C                   reset                   dscmz
056800010201     C                   move      vlrpoi        cmzdst
056900140117     C                   eval      cmzfld = 'EDIVATWR'
057000010201     C                   eval      cmzmbd = vlrhdl
057100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057200021009     C***                if        prmfir = *blanks
057300140117     C                   eval      cmzfla = 'EDIVAT0F'
057400140117     C                   eval      cmzmba = 'EDIVAT0F'
057500021009     C***                else
057600021009     C***                eval      cmzfla = prmfir
057700021009     C***                eval      cmzmba = prmfir
057800021009     C***                endif
057900010201     C                   eval      cmznrr = *zeros
058000020305     C                   move      §ctrokvt      cmznrr
058100021018     C                   eval      cmzlba = vlrfl1
058200010201     C                   call(e)   'TIS711C'
058300010201     C                   parm                    dscmz
058400010201     C                   parm      *blanks       esito
058500010205     C                   if        %error
058600010205     C                             or cmzerr = '1'
058700010205     C                             or esito  = '1'
058800010205     C                   eval      wrkesito = '3'
058900010205     C                   else
059000010201     C*
059100140117     C* 2° invio EDIVAB
059200010201     C                   reset                   dscmz
059300010201     C                   move      vlrpoi        cmzdst
059400010201     C                   eval      cmzfld = vlrfou
059500010201     C                   eval      cmzmbd = vlrhdl
059600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059700021009     C***                if        prmfir = *blanks
059800140117     C                   eval      cmzfla = 'EDIVAB0F'
059900140117     C                   eval      cmzmba = 'EDIVAB0F'
060000021009     C***                else
060100021009     C***                eval      cmzfla = prmfir
060200021009     C***                eval      cmzmba = prmfir
060300021009     C***                endif
060400010201     C                   eval      cmznrr = *zeros
060500010201     C                   move      §ctrokvb      cmznrr
060600021018     C                   eval      cmzlba = vlrfl1
060700010201     C                   call(e)   'TIS711C'
060800010201     C                   parm                    dscmz
060900010201     C                   parm      *blanks       esito
061000010201     C                   if        %error
061100010201     C                             or cmzerr = '1'
061200010201     C                             or esito  = '1'
061300010201     C                   eval      wrkesito = '3'
061400010201     C                   endif
061500010205     C                   endif
061600990920     C*
061700000613     C                   ENDSR
061800000613     C***
061900070411
062000070411     C     *pssr         BEGSR
062100070411     C*
062200070411     C                   if        %open(tivin00r)
062300070411     C                   close     tivin00r
062400070411     C                   endif
062500140117     C                   if        %open(edivabwr)
062600140117     C                   close     edivabwr
062700070411     C                   endif
062800140117     C                   if        %open(edivatwr)
062900140117     C                   close     edivatwr
063000070411     C                   endif
063100070411     C*
063200070411     C* Effettuo la chiamata al CLLE preposto
063300140117     C                   call(e)   'TITVEVTC'
063400070411     C                   parm                    parccm
063500070411     C                   parm                    parmbr
063600070411     C                   parm      '2'           paropz
063700070411     C*
063800070411     C                   eval      wrkesito = '2'
063900070411     C*
064000070411     C                   seton                                        LR
064100070411     C*
064200070411     C                   ENDSR     '*CANCL'
064300070411     C***
064400070411
064500990910
064600000613     C     *inzsr        BEGSR
064700990910     C*
064800990910     C     *entry        plist
064900990920     C                   parm                    tivlrds
065000990921     C                   parm      wrkesito      esito
065100000724     C                   parm                    prmlit
065200000710     C                   parm                    prmfir
065300000613     C*
065400000830     C* CALCOLA LA DATA CORRENTE
065500140117     C                   time                    wn14             14 0
065600140117     C                   movel     wn14          oracor            6 0          *ORA
065700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
065800100722     C                   eval      datcor = %dec(%date() : *ISO)
065900000830     C*
066000000613     C                   ENDSR
066100000613     C***
