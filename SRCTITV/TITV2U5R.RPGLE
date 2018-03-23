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
002700160321     D w250            s            250    INZ(*blanks)
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
021300160321     C                   Z-ADD     0120886       VABCCM
021400160321     C                   Z-ADD     0120886       VATCCM
021500160321     C                   Z-ADD     012           VABLNP
021600160321     C                   Z-ADD     012           VATLNP
021700160321     C                   Z-ADD     076           VABCTR
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
026600160321     C                   EVAL      savCMR = 'TESA '+%trim(%char(%date()))
026700140922     C***
026800160321     C* ...tipo record 'H' (testata)
026900160321     C                   IF        %trim(%subst(vindta:1:1)) = 'H'
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
032000160321     C* ......VABCCM
032100160321     C                   SELECT
032200160321     C                   WHEN      %trim(%subst(vindta:595:2)) = '10'
032300161122     C                   EVAL      VABCCM = 2221870
032400161122     C                   EVAL      VATCCM = 2221870
032500161122     C                   WHEN      %trim(%subst(vindta:595:2)) = '41'
032600161122     C                   EVAL      VABCCM = 2221869
032700161122     C                   EVAL      VATCCM = 2221869
032800160321     C                   ENDSL
032900160321     C* ......VABTSP / VABFFD
033000160321     C                   SELECT
033100160321     C                   WHEN      %trim(%subst(vindta:524:10)) = '14'
033200160321     C                   EVAL      VABTSP = 'H'
033300160321     C                   WHEN      %trim(%subst(vindta:524:10)) = '15'
033400160321     C                   EVAL      VABTSP = 'E'
033500160321     C                   WHEN      %trim(%subst(vindta:524:10)) = '07'
033600160321     C                   EVAL      VABFFD = 'S'
033700160321     C                   ENDSL
033800151119     C* ......VABRMA
033900160427     C***                EVAL      VABRMA=%trim(%subst(vindta:540:35))
034000150611     C* ......VABRSD
034100160321     C                   EVAL      VABRSD=%trim(%subst(vindta:35:35))
034200151119     C* ......VABRD2
034300160321     C                   EVAL      VABRD2=%trim(%subst(vindta:70:35))
034400150611     C* ......VABIND
034500160321     C                   EVAL      VABIND=%trim(%subst(vindta:105:35))+' '+
034600160321     C                                    %trim(%subst(vindta:140:35))
034700150611     C* ......VABCAD
034800160321     C                   EVAL      VABCAD=%trim(%subst(vindta:175:10))
034900150611     C* ......VABLOD
035000160321     C                   EVAL      VABLOD=%trim(%subst(vindta:185:35))
035100150611     C* ......VABPRD
035200160321     C                   EVAL      VABPRD=%trim(%subst(vindta:220:3))
035300150611     C* ......VABNZD
035400160321     C                   EVAL      VABNZD=%trim(%subst(vindta:223:3))
035500160321     C                   IF        VABNZD='IT' OR
035600160321     C                             VABNZD='ITA'
035700150611     C                   EVAL      VABNZD=*blanks
035800150611     C                   ENDIF
035900150611     C* ......VABPRD
036000150611     C* Reperisco la provincia dal CAP e dalla località
036100150611     C                   IF        VABPRD  = *blanks AND
036200150611     C                             VABLOD <> *blanks AND
036300150611     C                             VABCAD <> *blanks AND
036400150611     C                             VABNZD  = *blanks
036500150611     C                   CLEAR                   TISI95DS
036600150611     C                   EVAL      I95TCN = '3'
036700150611     C                   Z-ADD     datcor        I95DAT
036800150611     C                   EVAL      I95CAP = VABCAD
036900150611     C                   EVAL      I95LOC = VABLOD
037000150611     C                   CALL      'TISI95R'
037100150611     C                   PARM                    TISI95DS
037200150611     C                   EVAL      VABPRD = O95PRV
037300150611     C                   ENDIF
037400160215     C* ......VABNOT / VABNT2
037500160215     C* VABNOT e NT2 li costruisco come concat fi 2 stringhe MA, se queste risultano vuote, prendo
037600160215     C* il dato da una terza
037700160321     C                   EVAL      w250 = %trim(%subst(vindta:259:250))
037800160321     C                   EVAL      VABNOT = %subst(w250:1:35)
037900160321     C                   EVAL      VABNT2 = %subst(w250:36:35)
038000160321     C* ......VABDCR
038100160321     C                   IF        %trim(%subst(vindta:524:10))='11'
038200160321     C                   EVAL      VABTCR = *blanks
038300160321     C                   ELSE
038400160321     C                   EVAL      VABTCR = 'P'
038500160321     C                   ENDIF
038600160321     C                   EVAL      PiStr=%trim(%subst(vindta:226:8))
038700160321     C                   EXSR      CHKNUM
038800160321     C                   IF        PiInt=*on
038900160321     C                   Z-ADD     PiVal         VABDCR
039000160321     C                   ELSE
039100160321     C                   ADD       1             errore
039200160321     C                   EVAL      vinmsg = %trimr(vinmsg)
039300160321     C                             + ' ' + 'VABDCR'
039400160321     C                   ENDIF
039500151119     C* ......VABRMN
039600160321     C                   EVAL      PiStr=%trim(%subst(vindta:17:10))
039700151119     C                   EXSR      CHKNUM
039800151119     C                   IF        PiInt=*on
039900151119     C                   Z-ADD     PiVal         VABRMN
040000151119     C                   ELSE
040100151119     C                   ADD       1             errore
040200151119     C                   EVAL      vinmsg = %trimr(vinmsg)
040300151119     C                             + ' ' + 'VABRMN'
040400151119     C                   ENDIF
040500151119     C* ......VABPKB
040600160321     C                   EVAL      PiStr=%trim(%subst(vindta:239:10))
040700151119     C                   EXSR      CHKNUM
040800151119     C                   IF        PiNum=*on
040900160414     C***                EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
041000151119     C                   Z-ADD     PiVal         VABPKB
041100151119     C                   ELSE
041200151119     C                   ADD       1             errore
041300151119     C                   EVAL      vinmsg = %trimr(vinmsg)
041400151119     C                             + ' ' + 'VABPKB'
041500151119     C                   ENDIF
041600151119     C* ......VABNCL
041700160321     C                   EVAL      PiStr=%trim(%subst(vindta:234:5))
041800151119     C                   EXSR      CHKNUM
041900151119     C                   IF        PiInt=*on
042000151119     C                   ADD       PiVal         VABNCL
042100151119     C                   ELSE
042200151119     C                   ADD       1             errore
042300151119     C                   EVAL      vinmsg = %trimr(vinmsg)
042400151119     C                             + ' ' + 'VABNCL'
042500151119     C                   ENDIF
042600150611     C* ......VATNOT_B
042700160321     C                   EVAL      VATNOT=%trim(%subst(vindta:509:15))
042800150611     C                   IF        VATNOT <> *blanks
042900150611     C                   EVAL      VATCMR = VABCMR
043000150611     C                   EVAL      VATCNT = VABCNT
043100150611     C                   EVAL      VATTRC = 'B'
043200150611     C                   ADD       1             §CTROKVT
043300150611     C                   WRITE     EDIVAT00
043400150611     C                   ENDIF
043500160321     C* ......VATNOT_S
043600160321     C                   EVAL      VATNOT=%trim(%subst(vindta:509:15))
043700160321     C                   IF        VATNOT <> *blanks
043800160321     C                   EVAL      VATCMR = VABCMR
043900160321     C                   EVAL      VATCNT = VABCNT
044000160321     C                   EVAL      VATTRC = 'S'
044100160321     C                   ADD       1             §CTROKVT
044200160321     C                   WRITE     EDIVAT00
044300160321     C                   ENDIF
044400160321     C* ......VATNOT_IJ
044500160321     C                   IF        %trim(%subst(vindta:509:15)) = *blanks
044600160321     C                   EVAL      VATNOT=%trim(%subst(vindta:597:35))
044700160321     C                   IF        VATNOT <> *blanks
044800160321     C                   EVAL      VATCMR = VABCMR
044900160321     C                   EVAL      VATCNT = VABCNT
045000160321     C                   EVAL      VATTRC = 'I'
045100160321     C                   ADD       1             §CTROKVT
045200160321     C                   WRITE     EDIVAT00
045300160321     C                   ENDIF
045400160321     C                   EVAL      VATNOT=%trim(%subst(vindta:597+35:40-35))
045500160321     C                   IF        VATNOT <> *blanks
045600160321     C                   EVAL      VATCMR = VABCMR
045700160321     C                   EVAL      VATCNT = VABCNT
045800160321     C                   EVAL      VATTRC = 'J'
045900160321     C                   ADD       1             §CTROKVT
046000160321     C                   WRITE     EDIVAT00
046100160321     C                   ENDIF
046200160321     C                   ENDIF
046300140922     C* ......VATNOT_P
046400150611     C***                if        *in53
046500140922     C*
046600150611     C* Solo se valorizzato RMA
046700150611     C***                if        wRMA_1 <> *blanks
046800150611     C***                eval      pIn_MaskPDF =
046900150611     C***                                  %trim(%subst(vindta:1292:100))
047000150611     C***                call      'TIS7P2SER'
047100150611     C***                parm      'W'           pIn_Opz           1
047200150611     C***                parm                    tivlrds
047300150611     C***                parm                    EDIVABDS
047400150611     C***                parm      '10'          pIn_CdIdAz        2
047500150611     C***                parm                    pIn_MaskPDF      50
047600150611     C***                parm      *blanks       pOut_Esito        1
047700150611     C***                endif
047800150611     C***                endif
047900140117     C*
048000140117     C                   ENDIF
048100151119     C***
048200160321     C* ...tipo record 'D' (dettagli segnacollo)
048300160321     C                   IF        %trim(%subst(vindta:1:1)) = 'D'
048400151119     C* ......VATNOT_E
048500160321     C                   EVAL      VATNOT=%trim(%subst(vindta:12:20))
048600151119     C                   IF        VATNOT <> *blanks
048700151119     C                   EVAL      VATCMR = VABCMR
048800151119     C                   EVAL      VATCNT = VABCNT
048900151119     C                   EVAL      VATTRC = 'E'
049000151119     C                   ADD       1             §CTROKVT
049100151119     C                   WRITE     EDIVAT00
049200151119     C                   ENDIF
049300151119     C*
049400151119     C                   ENDIF
049500140117     C***
049600010202     C*
049700000801     C* Ebbene...
049800000801     C                   ADD       1             §CTRMO
049900010201     C                   IF        errore <> *zeros
050000000801     C                   ADD       1             §CTRNO
050100000801     C                   EVAL      vinflg = '2'
050200000801     C                   ELSE
050300010201     C                   ADD       1             §CTROKVB
050400000801     C                   ENDIF
050500000801     C*
050600000801     C                   ENDSR
050700010202     C*----------------------------------------------------*
050800140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
050900010202     C*----------------------------------------------------*
051000020305     C     PREVAT        BEGSR
051100010202     C*
051200140117     C* Compongo il nome del membro da dare al EDIVATWR
051300010202     C                   eval      parmbr = vlrhdl
051400010202     C                   movel     'M'           parmbr
051500060113     C                   eval      parccm = vlrksc
051600010202     C                   eval      paropz = '1'
051700010202     C* Effettuo la chiamata al CLLE preposto
051800140117     C                   call(e)   'TITVEVTC'
051900010202     C                   parm                    parccm
052000010202     C                   parm                    parmbr
052100010202     C                   parm                    paropz
052200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052300010202     C                   if        %error
052400010202     C                   movel     '1'           chkcall
052500010202     C                   else
052600010202     C                   movel     '0'           chkcall
052700010202     C                   endif
052800010202     C*
052900010202     C                   ENDSR
053000000801     C*----------------------------------------------------*
053100000801     C*  CONTROLLO NUMERICITA' CAMPI
053200000801     C*----------------------------------------------------*
053300000801     C     CHKNUM        BEGSR
053400000801     C*
053500000801     C                   call(e)   'ISNUMERIC'
053600000801     C                   PARM                    PiStr            30
053700040714     C                   PARM      ','           PiDecChr          1
053800000801     C                   PARM      *ZEROS        PiVal            30 9
053900000801     C                   PARM      '0'           PiInt             1
054000000801     C                   PARM      '0'           PiNum             1
054100000801     C                   IF        %error
054200000801     C                   EVAL      PiInt=*off
054300000801     C                   ENDIF
054400000801     C*
054500000801     C                   ENDSR
054600000801     C***
054700000801
054800011113
054900011113     C*----------------------------------------------------*
055000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055100011113     C*----------------------------------------------------*
055200011113     C     CHKIMPDIV     BEGSR
055300011113     C*
055400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055500011113     C                   Z-ADD     *zeros        wrkDec            9 9
055600011113     C*
055700011113     C* Come prima cosa effettuo considerazioni sulla divisa
055800011113     C                   IF        vabIAS > *zeros
055900011113     C                   IF        vabVAS <> 'EUR'
056000011113     C                   EVAL      vabVAS =  'ITL'
056100011113     C                   ENDIF
056200011113     C                   ENDIF
056300011113     C*
056400011113     C                   IF        vabCAS > *zeros
056500011113     C                   IF        vabVCA <> 'EUR'
056600011113     C                   EVAL      vabVCA =  'ITL'
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C                   IF        vabVMD > *zeros
057100020305     C                   IF        vabVAD <> 'EUR'
057200011113     C                   EVAL      vabVAD =  'ITL'
057300011113     C                   ENDIF
057400011113     C                   ENDIF
057500011113     C*
057600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057700011113     C                   Z-ADD     vabIAS        wrkDec
057800011113     C                   IF        wrkDec > *zeros
057900011113     C                   IF        vabVAS = 'ITL'
058000011113     C                   EVAL      vabIAS = *zeros
058100011113     C                   ENDIF
058200011113     C                   ENDIF
058300011113     C*
058400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058500011113     C                   Z-ADD     vabCAS        wrkDec
058600011113     C                   IF        wrkDec > *zeros
058700011113     C                   IF        vabVCA = 'ITL'
058800011113     C                   EVAL      vabCAS = *zeros
058900011113     C                   ENDIF
059000011113     C                   ENDIF
059100011113     C*
059200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059300011113     C                   Z-ADD     vabVMD        wrkDec
059400011113     C                   IF        wrkDec > *zeros
059500011113     C                   IF        vabVAD = 'ITL'
059600011113     C                   EVAL      vabVMD = *zeros
059700011113     C                   ENDIF
059800011113     C                   ENDIF
059900011113     C*
060000011113     C                   ENDSR
060100011113     C***
060200011113
060300011113
060400000801
060500000801
060600990920      /TITLE Invio dei dati al punto operativo.
060700010202     C     invio         BEGSR
060800990920     C*
060900140117     C* 1° invio EDIVAT
061000010201     C                   reset                   dscmz
061100010201     C                   move      vlrpoi        cmzdst
061200140117     C                   eval      cmzfld = 'EDIVATWR'
061300010201     C                   eval      cmzmbd = vlrhdl
061400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061500021009     C***                if        prmfir = *blanks
061600140117     C                   eval      cmzfla = 'EDIVAT0F'
061700140117     C                   eval      cmzmba = 'EDIVAT0F'
061800021009     C***                else
061900021009     C***                eval      cmzfla = prmfir
062000021009     C***                eval      cmzmba = prmfir
062100021009     C***                endif
062200010201     C                   eval      cmznrr = *zeros
062300020305     C                   move      §ctrokvt      cmznrr
062400021018     C                   eval      cmzlba = vlrfl1
062500010201     C                   call(e)   'TIS711C'
062600010201     C                   parm                    dscmz
062700010201     C                   parm      *blanks       esito
062800010205     C                   if        %error
062900010205     C                             or cmzerr = '1'
063000010205     C                             or esito  = '1'
063100010205     C                   eval      wrkesito = '3'
063200010205     C                   else
063300010201     C*
063400140117     C* 2° invio EDIVAB
063500010201     C                   reset                   dscmz
063600010201     C                   move      vlrpoi        cmzdst
063700010201     C                   eval      cmzfld = vlrfou
063800010201     C                   eval      cmzmbd = vlrhdl
063900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064000021009     C***                if        prmfir = *blanks
064100140117     C                   eval      cmzfla = 'EDIVAB0F'
064200140117     C                   eval      cmzmba = 'EDIVAB0F'
064300021009     C***                else
064400021009     C***                eval      cmzfla = prmfir
064500021009     C***                eval      cmzmba = prmfir
064600021009     C***                endif
064700010201     C                   eval      cmznrr = *zeros
064800010201     C                   move      §ctrokvb      cmznrr
064900021018     C                   eval      cmzlba = vlrfl1
065000010201     C                   call(e)   'TIS711C'
065100010201     C                   parm                    dscmz
065200010201     C                   parm      *blanks       esito
065300010201     C                   if        %error
065400010201     C                             or cmzerr = '1'
065500010201     C                             or esito  = '1'
065600010201     C                   eval      wrkesito = '3'
065700010201     C                   endif
065800010205     C                   endif
065900990920     C*
066000000613     C                   ENDSR
066100000613     C***
066200070411
066300070411     C     *pssr         BEGSR
066400070411     C*
066500070411     C                   if        %open(tivin00r)
066600070411     C                   close     tivin00r
066700070411     C                   endif
066800140117     C                   if        %open(edivabwr)
066900140117     C                   close     edivabwr
067000070411     C                   endif
067100140117     C                   if        %open(edivatwr)
067200140117     C                   close     edivatwr
067300070411     C                   endif
067400070411     C*
067500070411     C* Effettuo la chiamata al CLLE preposto
067600140117     C                   call(e)   'TITVEVTC'
067700070411     C                   parm                    parccm
067800070411     C                   parm                    parmbr
067900070411     C                   parm      '2'           paropz
068000070411     C*
068100070411     C                   eval      wrkesito = '2'
068200070411     C*
068300070411     C                   seton                                        LR
068400070411     C*
068500070411     C                   ENDSR     '*CANCL'
068600070411     C***
068700070411
068800990910
068900000613     C     *inzsr        BEGSR
069000990910     C*
069100990910     C     *entry        plist
069200990920     C                   parm                    tivlrds
069300990921     C                   parm      wrkesito      esito
069400000724     C                   parm                    prmlit
069500000710     C                   parm                    prmfir
069600000613     C*
069700000830     C* CALCOLA LA DATA CORRENTE
069800140117     C                   time                    wn14             14 0
069900140117     C                   movel     wn14          oracor            6 0          *ORA
070000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070100100722     C                   eval      datcor = %dec(%date() : *ISO)
070200000830     C*
070300000613     C                   ENDSR
070400000613     C***
