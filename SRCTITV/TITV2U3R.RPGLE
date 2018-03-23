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
002700160215     D w70             s             70    INZ(*blanks)
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
005900170731     C*
006000170731     C                   if        *in53
006100170731     C                   call      'TIS7P2SER'
006200170731     C                   parm      'C'           pIn_Opz           1
006300170731     C                   parm                    tivlrds
006400170731     C                   parm                    EDIVABDS
006500170731     C                   parm      *blanks       pIn_CdIdAz        2
006600170731     C                   parm      *blanks       pIn_MaskPDF      50
006700170731     C                   parm      *blanks       pOut_Esito        1
006800170731     C                   endif
006900000616     C*
007000010201     C                   seton                                        LR
007100990908
007200000801
007300910830     C*--------------------------------------------------------
007400140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007500910830     C*--------------------------------------------------------
007600040526     C     RWFILE        BEGSR
007700990910     C*
007800990914     C                   if        not %open(tivin00r)
007900990908     C                   open      tivin00r
008000990914     C                   endif
008100140117     C                   if        not %open(edivabwr)
008200140117     C                   open      edivabwr
008300990914     C                   endif
008400140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008500020305     C                   exsr      prevat
008600010201     C*
008700010202     C                   if        chkcall = '0'
008800010202     C*
008900140117     C                   if        not %open(edivatwr)
009000140117     C                   open      edivatwr
009100010201     C                   endif
009200990910     C*
009300010201     C                   clear                   §CTROKVB          5 0
009400020305     C                   clear                   §CTROKVT          5 0
009500000801     C                   clear                   §CTRMO            5 0
009600000801     C                   clear                   §CTRNO            5 0
009700100729     C*
009800100729     C                   z-add     1             wGiro             1 0
009900140117     C*
010000140902     C                   if        *in53
010100140117     C                   call      'TIS7P2SER'
010200140117     C                   parm      'O'           pIn_Opz           1
010300140117     C                   parm                    tivlrds
010400140117     C                   parm                    EDIVABDS
010500140117     C                   parm      *blanks       pIn_CdIdAz        2
010600140117     C                   parm      *blanks       pIn_MaskPDF      50
010700140117     C                   parm      *blanks       pOut_Esito        1
010800140902     C                   endif
010900130708     C*
011000130708     C                   SETOFF                                       31
011100921023     C                   DO        *HIVAL
011200990913     C*
011300990915     C                   READ      tivin00r                               70
011400040910     C                   if        vindta > *blanks
011500000613     C                   add       1             rrnum
011600000801     C*
011700000801     C                   if        *in70 = *off
011800000801     C                             and
011900000801     C                             (vinflg = *blanks
012000000801     C                              or vinflg = '0'
012100000801     C                              or vinflg = '2')
012200000801     C*
012300000801     C                   clear                   vinmsg
012400000801     C                   eval      vinflg = '1'
012500040910     C*
012600040910     C* Eseguo routine d traduzione
012700040910     C                   exsr      impvabvat
012800040802     C*
012900010305     C                   endif
013000000905     C*
013100000905     C                   else
013200000905     C                   eval      vinflg = '1'
013300000905     C                   endif
013400000905     C*
013500000905     C  N70              update    tivin000
013600000905     C*
013700991022     C  N70              ENDdo
013800100722     C*
013900100722     C* Scarico i buffer testata ancora "in canna"
014000140922     C*
014100140922     C  N31              exsr      CHKWRI
014200140117     C  N31              WRITE     EDIVAB00
014300130708     C                   SETOFF                                       31
014400010202     C*
014500010202     C                   endif
014600990910
014700990910     C* Se non ci sono record con errori ...
014800000710     C                   if        §ctrno = 0
014900990910     C* ... restituisco esito OK.
015000990921     C                   eval      wrkesito = '0'
015100990910     C                   else
015200010201     C                   if        §ctrokvb > 0
015300990921     C                   eval      wrkesito = '1'
015400000710     C                   else
015500000710     C                   eval      wrkesito = '2'
015600990910     C                   endif
015700000710     C                   endif
015800990910     C*
015900990914     C                   if        %open(tivin00r)
016000990908     C                   close     tivin00r
016100990914     C                   endif
016200140117     C                   if        %open(edivabwr)
016300140117     C                   close     edivabwr
016400990914     C                   endif
016500140117     C                   if        %open(edivatwr)
016600140117     C                   close     edivatwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   if        §ctrokvb > 0
017000000724     C                             and vlrpoi <> *zeros
017100010202     C                   exsr      invio
017200990920     C                   endif
017300990920     C*
017400910830     C                   ENDSR
017500000613     C***
017600140922
017700140922     C*----------------------------------------------------*
017800140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
017900140922     C*----------------------------------------------------*
018000140922     C     CHKWRI        BEGSR
018100140922     C*
018200140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018300140922     C                   IF        FlgCAS <> '0'
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
021300151119     C                   Z-ADD     0054876       VABCCM
021400151119     C                   Z-ADD     0054876       VATCCM
021500151119     C                   Z-ADD     005           VABLNP
021600151119     C                   Z-ADD     005           VATLNP
021700151119     C                   Z-ADD     300           VABCTR
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
026000040910     C*
026100040910     C* Traduzione relativa ai tipi record del file del cliente
026200040910     C*
026300071210     C*
026400071210     C***
026500151119     C                   MOVEL     *blanks       savCMR           35
026600151119     C                   EVAL      savCMR = %trim(%subst(vlrmsg:51:30))
026700140922     C***
026800151119     C* ...tipo record '2' (testata)
026900151119     C                   IF        %trim(%subst(vindta:1:1)) = '2'
027000100722     C*
027100100722     C* Se nn primo giro => scarico il buffer precedente
027200150611     C                   if        wGiro = 1
027300100722     C                   eval      wGiro = 2
027400100722     C                   else
027500140922     C*
027600140922     C  N31              exsr      CHKWRI
027700140117     C  N31              WRITE     EDIVAB00
027800130708     C                   SETOFF                                       31
027900100722     C                   endif
028000100722     C*
028100071210     C* Resetto indicatore d anomalia sul singolo record
028200071210     C                   eval      vinflg = '1'
028300071210     C* ......inizializzazioni iniziali e formati record file Bartolini
028400071210     C                   EXSR      INZVAR
028500071210     C                   EXSR      DEFCAM
028600071210     C*
028700071210     C                   Z-ADD     *zeros        errore            1 0
028800071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
028900071210     C                   MOVEL     datcor        VABAAS
029000071210     C                   MOVEL     datcor        VATAAS
029100071210     C                   MOVE      datcor        VABMGS
029200071210     C                   MOVE(P)   vlrpoi        VABFGS
029300071210     C                   MOVE(P)   vlrpoi        VATFGS
029400140903     C*
029500140903     C* Dai "fissi"
029600140903     C                   EVAL      VABCMR = savCMR
029700140903     C                   EVAL      VABDCM = datcor
029800140903     C                   EVAL      VABDTS = datcor
029900140903     C                   EVAL      VABHMS = oracor
030000140903     C                   EVAL      VABCNT = 1
030100140903     C*
030200071210     C* ......VABNSP/VATNSP
030300071210     C* NSP => Stacco un numeratore da AZNUM
030400071210     C                   clear                   TRUL33DS
030500071210     C                   eval      I33OPE = *zeros
030600071210     C                   eval      I33CNU = 302
030700071210     C                   eval      I33NUM = 1
030800071210     C                   movel     TRUL33DS      KPJBU
030900071210     C                   call      'TRUL33R'
031000071210     C                   parm                    KPJBA
031100071210     C                   movel     KPJBU         TRUL33DS
031200071210     C                   if        O33ERR = *zeros
031300071210     C                   z-add     O33NRF        VABNSP
031400071210     C                   z-add     O33NRF        VATNSP
031500071210     C                   else
031600071210     C                   Z-ADD     1             errore
031700071210     C                   EVAL      vinmsg = %trimr(vinmsg)
031800071210     C                             + ' ' + 'VABNSP VATNSP'
031900071210     C                   endif
032000151119     C* ......VABRMA
032100151119     C                   EVAL      VABRMA=%trim(%subst(vindta:11:7))
032200150611     C* ......VABRSD
032300151119     C                   EVAL      VABRSD=%trim(%subst(vindta:19:35))
032400151119     C* ......VABRD2
032500151119     C                   EVAL      VABRD2=%trim(%subst(vindta:54:35))
032600150611     C* ......VABIND
032700151119     C                   EVAL      VABIND=%trim(%subst(vindta:89:35))
032800150611     C* ......VABCAD
032900151119     C                   EVAL      VABCAD=%trim(%subst(vindta:124:9))
033000150611     C* ......VABLOD
033100151119     C                   EVAL      VABLOD=%trim(%subst(vindta:133:35))
033200150611     C* ......VABPRD
033300151119     C                   EVAL      VABPRD=%trim(%subst(vindta:168:2))
033400160419     C* ......VABTC1
033500160419     C                   EVAL      VABTC1=%trim(%subst(vindta:374:1))
033600150611     C* ......VABNZD
033700151119     C                   EVAL      VABNZD=%trim(%subst(vindta:171:3))
033800150611     C                   IF        VABNZD='IT'
033900150611     C                   EVAL      VABNZD=*blanks
034000150611     C                   ENDIF
034100150611     C* ......VABPRD
034200150611     C* Reperisco la provincia dal CAP e dalla località
034300150611     C                   IF        VABPRD  = *blanks AND
034400150611     C                             VABLOD <> *blanks AND
034500150611     C                             VABCAD <> *blanks AND
034600150611     C                             VABNZD  = *blanks
034700150611     C                   CLEAR                   TISI95DS
034800150611     C                   EVAL      I95TCN = '3'
034900150611     C                   Z-ADD     datcor        I95DAT
035000150611     C                   EVAL      I95CAP = VABCAD
035100150611     C                   EVAL      I95LOC = VABLOD
035200150611     C                   CALL      'TISI95R'
035300150611     C                   PARM                    TISI95DS
035400150611     C                   EVAL      VABPRD = O95PRV
035500150611     C                   ENDIF
035600160215     C* ......VABNOT / VABNT2
035700160215     C* VABNOT e NT2 li costruisco come concat fi 2 stringhe MA, se queste risultano vuote, prendo
035800160215     C* il dato da una terza
035900160215     C                   EVAL      w70 = *blank
036000160215     C                   EVAL      w70=%trim(%subst(vindta:304:35)) +
036100160215     C                                 %trim(%subst(vindta:339:35))
036200160215     C                   IF        w70 = *blank
036300160215     C                   EVAL      w70=%trim(%subst(vindta:283:20))
036400160215     C                   ENDIF
036500160215     C                   EVAL      VABNOT=%subst(w70:1:35)
036600160215     C                   EVAL      VABNT2=%subst(w70:36:35)
036700151119     C* ......VABRMN
036800151119     C                   EVAL      PiStr=%trim(%subst(vindta:225:15))
036900151119     C                   EXSR      CHKNUM
037000151119     C                   IF        PiInt=*on
037100151119     C                   Z-ADD     PiVal         VABRMN
037200151119     C                   ELSE
037300151119     C                   ADD       1             errore
037400151119     C                   EVAL      vinmsg = %trimr(vinmsg)
037500151119     C                             + ' ' + 'VABRMN'
037600151119     C                   ENDIF
037700151119     C* ......VABPKB
037800151119     C                   EVAL      PiStr=%trim(%subst(vindta:181:9))
037900151119     C                   EXSR      CHKNUM
038000151119     C                   IF        PiNum=*on
038100151119     C                   Z-ADD     PiVal         VABPKB
038200151119     C                   ELSE
038300151119     C                   ADD       1             errore
038400151119     C                   EVAL      vinmsg = %trimr(vinmsg)
038500151119     C                             + ' ' + 'VABPKB'
038600151119     C                   ENDIF
038700151119     C* ......VABVLB
038800151119     C                   EVAL      PiStr=%trim(%subst(vindta:191:7))
038900151119     C                   EXSR      CHKNUM
039000151119     C                   IF        PiNum=*on
039100151119     C                   Z-ADD     PiVal         VABVLB
039200151119     C                   ELSE
039300151119     C                   ADD       1             errore
039400151119     C                   EVAL      vinmsg = %trimr(vinmsg)
039500151119     C                             + ' ' + 'VABVLB'
039600151119     C                   ENDIF
039700151119     C* ......VABNCL
039800151119     C                   EVAL      PiStr=%trim(%subst(vindta:175:5))
039900151119     C                   EXSR      CHKNUM
040000151119     C                   IF        PiInt=*on
040100151119     C                   ADD       PiVal         VABNCL
040200151119     C                   ELSE
040300151119     C                   ADD       1             errore
040400151119     C                   EVAL      vinmsg = %trimr(vinmsg)
040500151119     C                             + ' ' + 'VABNCL'
040600151119     C                   ENDIF
040700150611     C* ......VATNOT_B
040800160215     C* ho due possibili scelte, con priorità se c'è il dato da pos. 283 x 20
040900160215     C                   IF        %subst(vindta:283:20) <> *blank
041000160215     C                   EVAL      VATNOT=%trim(%subst(vindta:283:20))
041100160215     C                   ELSE
041200160215     C                   EVAL      VATNOT=%trim(%subst(vindta:262:20))
041300160215     C                   ENDIF
041400150611     C                   IF        VATNOT <> *blanks
041500150611     C                   EVAL      VATCMR = VABCMR
041600150611     C                   EVAL      VATCNT = VABCNT
041700150611     C                   EVAL      VATTRC = 'B'
041800150611     C                   ADD       1             §CTROKVT
041900150611     C                   WRITE     EDIVAT00
042000150611     C                   ENDIF
042100151119     C* ......VATNOT_S
042200151119     C                   IF        %trim(%subst(vindta:241:20)) <> *blanks
042300151127     C                   EVAL      VATNOT=%trim(%subst(vindta:241:20))
042400151119     C                   ELSE
042500151119     C                   EVAL      VATNOT=%trim(%subst(vindta:283:20))
042600151119     C                   ENDIF
042700150611     C                   IF        VATNOT <> *blanks
042800150611     C                   EVAL      VATCMR = VABCMR
042900150611     C                   EVAL      VATCNT = VABCNT
043000151119     C                   EVAL      VATTRC = 'S'
043100150921     C                   ADD       1             §CTROKVT
043200160203     C*** il cliente non vuole più l'invio dell'alert SMS
043300160203     C***                WRITE     EDIVAT00
043400150611     C                   ENDIF
043500150611     C* ......VABCAS
043600151119     C                   IF        %trim(%subst(vindta:199:15)) <> *all'0' AND
043700151119     C                             %trim(%subst(vindta:199:15)) <> '0,00'
043800151119     C                   EVAL      FlgCAS = '1'
043900151201     C                   EVAL      VABTIC = 'BM'
044000151119     C                   EVAL      VABVCA = %trim(%subst(vindta:221:3))
044100151119     C                   EVAL      PiStr=%trim(%subst(vindta:199:15))
044200151119     C                   EXSR      CHKNUM
044300151119     C                   IF        PiNum=*on
044400151119     C                   Z-ADD     PiVal         VABCAS
044500151119     C                   ELSE
044600151119     C                   ADD       1             errore
044700151119     C                   EVAL      vinmsg = %trimr(vinmsg)
044800151119     C                             + ' ' + 'VABCAS'
044900151119     C                   ENDIF
045000151119     C                   ENDIF
045100140922     C* ......VATNOT_P
045200150611     C***                if        *in53
045300140922     C*
045400150611     C* Solo se valorizzato RMA
045500150611     C***                if        wRMA_1 <> *blanks
045600150611     C***                eval      pIn_MaskPDF =
045700150611     C***                                  %trim(%subst(vindta:1292:100))
045800150611     C***                call      'TIS7P2SER'
045900150611     C***                parm      'W'           pIn_Opz           1
046000150611     C***                parm                    tivlrds
046100150611     C***                parm                    EDIVABDS
046200150611     C***                parm      '10'          pIn_CdIdAz        2
046300150611     C***                parm                    pIn_MaskPDF      50
046400150611     C***                parm      *blanks       pOut_Esito        1
046500150611     C***                endif
046600150611     C***                endif
046700140117     C*
046800140117     C                   ENDIF
046900151119     C***
047000151119     C* ...tipo record 'E' (dettagli segnacollo)
047100151119     C                   IF        %trim(%subst(vindta:1:1)) = 'E'
047200151119     C* ......VATNOT_E
047300151119     C                   EVAL      VATNOT=%trim(%subst(vindta:2:10))
047400151119     C                   IF        VATNOT <> *blanks
047500151119     C                   EVAL      VATCMR = VABCMR
047600151119     C                   EVAL      VATCNT = VABCNT
047700151119     C                   EVAL      VATTRC = 'E'
047800151119     C                   ADD       1             §CTROKVT
047900151119     C                   WRITE     EDIVAT00
048000151119     C                   ENDIF
048100151119     C*
048200151119     C                   ENDIF
048300140117     C***
048400010202     C*
048500000801     C* Ebbene...
048600000801     C                   ADD       1             §CTRMO
048700010201     C                   IF        errore <> *zeros
048800000801     C                   ADD       1             §CTRNO
048900000801     C                   EVAL      vinflg = '2'
049000000801     C                   ELSE
049100010201     C                   ADD       1             §CTROKVB
049200000801     C                   ENDIF
049300000801     C*
049400000801     C                   ENDSR
049500010202     C*----------------------------------------------------*
049600140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049700010202     C*----------------------------------------------------*
049800020305     C     PREVAT        BEGSR
049900010202     C*
050000140117     C* Compongo il nome del membro da dare al EDIVATWR
050100010202     C                   eval      parmbr = vlrhdl
050200010202     C                   movel     'M'           parmbr
050300060113     C                   eval      parccm = vlrksc
050400010202     C                   eval      paropz = '1'
050500010202     C* Effettuo la chiamata al CLLE preposto
050600140117     C                   call(e)   'TITVEVTC'
050700010202     C                   parm                    parccm
050800010202     C                   parm                    parmbr
050900010202     C                   parm                    paropz
051000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051100010202     C                   if        %error
051200010202     C                   movel     '1'           chkcall
051300010202     C                   else
051400010202     C                   movel     '0'           chkcall
051500010202     C                   endif
051600010202     C*
051700010202     C                   ENDSR
051800000801     C*----------------------------------------------------*
051900000801     C*  CONTROLLO NUMERICITA' CAMPI
052000000801     C*----------------------------------------------------*
052100000801     C     CHKNUM        BEGSR
052200000801     C*
052300000801     C                   call(e)   'ISNUMERIC'
052400000801     C                   PARM                    PiStr            30
052500040714     C                   PARM      ','           PiDecChr          1
052600000801     C                   PARM      *ZEROS        PiVal            30 9
052700000801     C                   PARM      '0'           PiInt             1
052800000801     C                   PARM      '0'           PiNum             1
052900000801     C                   IF        %error
053000000801     C                   EVAL      PiInt=*off
053100000801     C                   ENDIF
053200000801     C*
053300000801     C                   ENDSR
053400000801     C***
053500000801
053600011113
053700011113     C*----------------------------------------------------*
053800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053900011113     C*----------------------------------------------------*
054000011113     C     CHKIMPDIV     BEGSR
054100011113     C*
054200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054300011113     C                   Z-ADD     *zeros        wrkDec            9 9
054400011113     C*
054500011113     C* Come prima cosa effettuo considerazioni sulla divisa
054600011113     C                   IF        vabIAS > *zeros
054700011113     C                   IF        vabVAS <> 'EUR'
054800011113     C                   EVAL      vabVAS =  'ITL'
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C                   IF        vabCAS > *zeros
055300011113     C                   IF        vabVCA <> 'EUR'
055400011113     C                   EVAL      vabVCA =  'ITL'
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C                   IF        vabVMD > *zeros
055900020305     C                   IF        vabVAD <> 'EUR'
056000011113     C                   EVAL      vabVAD =  'ITL'
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056500011113     C                   Z-ADD     vabIAS        wrkDec
056600011113     C                   IF        wrkDec > *zeros
056700011113     C                   IF        vabVAS = 'ITL'
056800011113     C                   EVAL      vabIAS = *zeros
056900011113     C                   ENDIF
057000011113     C                   ENDIF
057100011113     C*
057200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057300011113     C                   Z-ADD     vabCAS        wrkDec
057400011113     C                   IF        wrkDec > *zeros
057500011113     C                   IF        vabVCA = 'ITL'
057600011113     C                   EVAL      vabCAS = *zeros
057700011113     C                   ENDIF
057800011113     C                   ENDIF
057900011113     C*
058000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058100011113     C                   Z-ADD     vabVMD        wrkDec
058200011113     C                   IF        wrkDec > *zeros
058300011113     C                   IF        vabVAD = 'ITL'
058400011113     C                   EVAL      vabVMD = *zeros
058500011113     C                   ENDIF
058600011113     C                   ENDIF
058700011113     C*
058800011113     C                   ENDSR
058900011113     C***
059000011113
059100011113
059200000801
059300000801
059400990920      /TITLE Invio dei dati al punto operativo.
059500010202     C     invio         BEGSR
059600990920     C*
059700140117     C* 1° invio EDIVAT
059800010201     C                   reset                   dscmz
059900010201     C                   move      vlrpoi        cmzdst
060000140117     C                   eval      cmzfld = 'EDIVATWR'
060100010201     C                   eval      cmzmbd = vlrhdl
060200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060300021009     C***                if        prmfir = *blanks
060400140117     C                   eval      cmzfla = 'EDIVAT0F'
060500140117     C                   eval      cmzmba = 'EDIVAT0F'
060600021009     C***                else
060700021009     C***                eval      cmzfla = prmfir
060800021009     C***                eval      cmzmba = prmfir
060900021009     C***                endif
061000010201     C                   eval      cmznrr = *zeros
061100020305     C                   move      §ctrokvt      cmznrr
061200021018     C                   eval      cmzlba = vlrfl1
061300010201     C                   call(e)   'TIS711C'
061400010201     C                   parm                    dscmz
061500010201     C                   parm      *blanks       esito
061600010205     C                   if        %error
061700010205     C                             or cmzerr = '1'
061800010205     C                             or esito  = '1'
061900010205     C                   eval      wrkesito = '3'
062000010205     C                   else
062100010201     C*
062200140117     C* 2° invio EDIVAB
062300010201     C                   reset                   dscmz
062400010201     C                   move      vlrpoi        cmzdst
062500010201     C                   eval      cmzfld = vlrfou
062600010201     C                   eval      cmzmbd = vlrhdl
062700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062800021009     C***                if        prmfir = *blanks
062900140117     C                   eval      cmzfla = 'EDIVAB0F'
063000140117     C                   eval      cmzmba = 'EDIVAB0F'
063100021009     C***                else
063200021009     C***                eval      cmzfla = prmfir
063300021009     C***                eval      cmzmba = prmfir
063400021009     C***                endif
063500010201     C                   eval      cmznrr = *zeros
063600010201     C                   move      §ctrokvb      cmznrr
063700021018     C                   eval      cmzlba = vlrfl1
063800010201     C                   call(e)   'TIS711C'
063900010201     C                   parm                    dscmz
064000010201     C                   parm      *blanks       esito
064100010201     C                   if        %error
064200010201     C                             or cmzerr = '1'
064300010201     C                             or esito  = '1'
064400010201     C                   eval      wrkesito = '3'
064500010201     C                   endif
064600010205     C                   endif
064700990920     C*
064800000613     C                   ENDSR
064900000613     C***
065000070411
065100070411     C     *pssr         BEGSR
065200070411     C*
065300070411     C                   if        %open(tivin00r)
065400070411     C                   close     tivin00r
065500070411     C                   endif
065600140117     C                   if        %open(edivabwr)
065700140117     C                   close     edivabwr
065800070411     C                   endif
065900140117     C                   if        %open(edivatwr)
066000140117     C                   close     edivatwr
066100070411     C                   endif
066200070411     C*
066300070411     C* Effettuo la chiamata al CLLE preposto
066400140117     C                   call(e)   'TITVEVTC'
066500070411     C                   parm                    parccm
066600070411     C                   parm                    parmbr
066700070411     C                   parm      '2'           paropz
066800070411     C*
066900070411     C                   eval      wrkesito = '2'
067000070411     C*
067100070411     C                   seton                                        LR
067200070411     C*
067300070411     C                   ENDSR     '*CANCL'
067400070411     C***
067500070411
067600990910
067700000613     C     *inzsr        BEGSR
067800990910     C*
067900990910     C     *entry        plist
068000990920     C                   parm                    tivlrds
068100990921     C                   parm      wrkesito      esito
068200000724     C                   parm                    prmlit
068300000710     C                   parm                    prmfir
068400000613     C*
068500000830     C* CALCOLA LA DATA CORRENTE
068600140117     C                   time                    wn14             14 0
068700140117     C                   movel     wn14          oracor            6 0          *ORA
068800100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068900100722     C                   eval      datcor = %dec(%date() : *ISO)
069000000830     C*
069100000613     C                   ENDSR
069200000613     C***
