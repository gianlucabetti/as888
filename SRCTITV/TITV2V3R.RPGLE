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
005700170731     C*
005800170731     C                   if        *in53
005900170731     C                   call      'TIS7P2SER'
006000170731     C                   parm      'C'           pIn_Opz           1
006100170731     C                   parm                    tivlrds
006200170731     C                   parm                    EDIVABDS
006300170731     C                   parm      *blanks       pIn_CdIdAz        2
006400170731     C                   parm      *blanks       pIn_MaskPDF      50
006500170731     C                   parm      *blanks       pOut_Esito        1
006600170731     C                   endif
006700000616     C*
006800010201     C                   seton                                        LR
006900990908
007000000801
007100910830     C*--------------------------------------------------------
007200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007300910830     C*--------------------------------------------------------
007400040526     C     RWFILE        BEGSR
007500990910     C*
007600990914     C                   if        not %open(tivin00r)
007700990908     C                   open      tivin00r
007800990914     C                   endif
007900140117     C                   if        not %open(edivabwr)
008000140117     C                   open      edivabwr
008100990914     C                   endif
008200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008300020305     C                   exsr      prevat
008400010201     C*
008500010202     C                   if        chkcall = '0'
008600010202     C*
008700140117     C                   if        not %open(edivatwr)
008800140117     C                   open      edivatwr
008900010201     C                   endif
009000990910     C*
009100010201     C                   clear                   §CTROKVB          5 0
009200020305     C                   clear                   §CTROKVT          5 0
009300000801     C                   clear                   §CTRMO            5 0
009400000801     C                   clear                   §CTRNO            5 0
009500100729     C*
009600100729     C                   z-add     1             wGiro             1 0
009700140117     C*
009800140902     C                   if        *in53
009900140117     C                   call      'TIS7P2SER'
010000140117     C                   parm      'O'           pIn_Opz           1
010100140117     C                   parm                    tivlrds
010200140117     C                   parm                    EDIVABDS
010300140117     C                   parm      *blanks       pIn_CdIdAz        2
010400140117     C                   parm      *blanks       pIn_MaskPDF      50
010500140117     C                   parm      *blanks       pOut_Esito        1
010600140902     C                   endif
010700130708     C*
010800161003     C* ......inizializzazioni iniziali e formati record file Bartolini
010900161003     C                   EXSR      INZVAR
011000161003     C                   EXSR      DEFCAM
011100161003     C*
011200130708     C                   SETOFF                                       31
011300921023     C                   DO        *HIVAL
011400990913     C*
011500990915     C                   READ      tivin00r                               70
011600040910     C                   if        vindta > *blanks
011700000613     C                   add       1             rrnum
011800000801     C*
011900000801     C                   if        *in70 = *off
012000000801     C                             and
012100000801     C                             (vinflg = *blanks
012200000801     C                              or vinflg = '0'
012300000801     C                              or vinflg = '2')
012400000801     C*
012500000801     C                   clear                   vinmsg
012600000801     C                   eval      vinflg = '1'
012700040910     C*
012800040910     C* Eseguo routine d traduzione
012900040910     C                   exsr      impvabvat
013000040802     C*
013100010305     C                   endif
013200000905     C*
013300000905     C                   else
013400000905     C                   eval      vinflg = '1'
013500000905     C                   endif
013600000905     C*
013700000905     C  N70              update    tivin000
013800000905     C*
013900991022     C  N70              ENDdo
014000100722     C*
014100100722     C* Scarico i buffer testata ancora "in canna"
014200140922     C*
014300140922     C  N31              exsr      CHKWRI
014400140117     C  N31              WRITE     EDIVAB00
014500130708     C                   SETOFF                                       31
014600010202     C*
014700010202     C                   endif
014800990910
014900990910     C* Se non ci sono record con errori ...
015000000710     C                   if        §ctrno = 0
015100990910     C* ... restituisco esito OK.
015200990921     C                   eval      wrkesito = '0'
015300990910     C                   else
015400010201     C                   if        §ctrokvb > 0
015500990921     C                   eval      wrkesito = '1'
015600000710     C                   else
015700000710     C                   eval      wrkesito = '2'
015800990910     C                   endif
015900000710     C                   endif
016000990910     C*
016100990914     C                   if        %open(tivin00r)
016200990908     C                   close     tivin00r
016300990914     C                   endif
016400140117     C                   if        %open(edivabwr)
016500140117     C                   close     edivabwr
016600990914     C                   endif
016700140117     C                   if        %open(edivatwr)
016800140117     C                   close     edivatwr
016900010201     C                   endif
017000990910     C*
017100010201     C                   if        §ctrokvb > 0
017200000724     C                             and vlrpoi <> *zeros
017300010202     C                   exsr      invio
017400990920     C                   endif
017500990920     C*
017600910830     C                   ENDSR
017700000613     C***
017800140922
017900140922     C*----------------------------------------------------*
018000140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
018100140922     C*----------------------------------------------------*
018200140922     C     CHKWRI        BEGSR
018300140922     C*
018400140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
018500140922     C                   IF        FlgCAS <> '0'
018600140922     C                   IF        VABCBO = '1'
018700140922     C                   EVAL      VABCBO = '4'
018800140922     C                   ELSE
018900140922     C                   EVAL      VABCBO = '6'
019000140922     C                   ENDIF
019100140922     C                   ENDIF
019200140922     C*
019300140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
019400140922     C                   EXSR      CHKIMPDIV
019500140922     C*
019600140922     C                   ENDSR
019700990920
019800000801     C*----------------------------------------------------*
019900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020000000801     C*----------------------------------------------------*
020100010201     C     INZVAR        BEGSR
020200000801     C*
020300040802     C                   Z-ADD     *zeros        Num5_0            5 0
020400040802     C                   MOVEL     '0'           FlgCAS            1
020500000801     C*
020600000801     C                   ENDSR
020700000801     C*----------------------------------------------------*
020800040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020900000801     C*----------------------------------------------------*
021000000801     C     DEFCAM        BEGSR
021100000801     C*
021200140117     C                   CLEAR                   EDIVAB00
021300140117     C                   CLEAR                   EDIVAT00
021400020619     C* Imposto i valori di default...
021500161003     C                   Z-ADD     1111488       VABCCM
021600161003     C                   Z-ADD     1111488       VATCCM
021700161003     C                   Z-ADD     111           VABLNP
021800161003     C                   Z-ADD     111           VATLNP
021900160509     C                   Z-ADD     000           VABCTR
022000151119     C                   MOVEL     '7Q'          VABCTM
022100040823     C                   MOVEL     '1'           VABCBO
022200020619     C* ... e poi verifico se sono stati passati come parametri
022300020619     C                   IF        vlrppt > *blanks
022400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022600020619     C                   EXSR      CHKNUM
022700020619     C                   IF        PiInt=*on
022800020619     C                   Z-ADD     PiVal         VABCCM
022900020619     C                   Z-ADD     PiVal         VATCCM
023000020619     C                   ENDIF
023100040506     C                   ENDIF
023200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023400020619     C                   EXSR      CHKNUM
023500020619     C                   IF        PiInt=*on
023600020619     C                   Z-ADD     PiVal         VABLNP
023700020619     C                   Z-ADD     PiVal         VATLNP
023800040506     C                   ENDIF
023900020619     C                   ENDIF
024000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024200020619     C                   EXSR      CHKNUM
024300020619     C                   IF        PiInt=*on
024400020619     C                   Z-ADD     PiVal         VABCTR
024500040506     C                   ENDIF
024600020619     C                   ENDIF
024700060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
024800060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
024900060202     C                   ENDIF
025000140902     C                   IF        %subst(vlrppt:16:1) = 'S'
025100140902     C                   SETON                                        53
025200140902     C                   ELSE
025300140902     C                   SETOFF                                       53
025400140902     C                   ENDIF
025500020619     C                   ENDIF
025600000801     C*
025700000801     C                   ENDSR
025800000801     C*----------------------------------------------------*
025900140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
026000000801     C*----------------------------------------------------*
026100040910     C     IMPVABVAT     BEGSR
026200040910     C*
026300040910     C* Traduzione relativa ai tipi record del file del cliente
026400040910     C*
026500071210     C*
026600071210     C***
026700151119     C                   MOVEL     *blanks       savCMR           35
026800161014     C                   EVAL      savCMR = 'YESFOOD - INNOCENTI '+
026900161003     C                                      %trim(%char(%date()))
027000140922     C***
027100161003     C* ...tipo record 'T1'/'T2' (testata)
027200161003     C                   IF        %trim(%subst(vindta:1:2)) = 'T1' OR
027300161003     C                             %trim(%subst(vindta:1:2)) = 'T2'
027400100722     C*
027500100722     C* Se nn primo giro => scarico il buffer precedente
027600150611     C                   if        wGiro = 1
027700100722     C                   eval      wGiro = 2
027800100722     C                   else
027900140922     C*
028000140922     C  N31              exsr      CHKWRI
028100140117     C  N31              WRITE     EDIVAB00
028200130708     C                   SETOFF                                       31
028300100722     C                   endif
028400100722     C*
028500071210     C* Resetto indicatore d anomalia sul singolo record
028600071210     C                   eval      vinflg = '1'
028700071210     C* ......inizializzazioni iniziali e formati record file Bartolini
028800071210     C                   EXSR      INZVAR
028900071210     C                   EXSR      DEFCAM
029000071210     C*
029100071210     C                   Z-ADD     *zeros        errore            1 0
029200071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
029300071210     C                   MOVEL     datcor        VABAAS
029400071210     C                   MOVEL     datcor        VATAAS
029500071210     C                   MOVE      datcor        VABMGS
029600071210     C                   MOVE(P)   vlrpoi        VABFGS
029700071210     C                   MOVE(P)   vlrpoi        VATFGS
029800140903     C*
029900140903     C* Dai "fissi"
030000140903     C                   EVAL      VABCMR = savCMR
030100140903     C                   EVAL      VABDCM = datcor
030200140903     C                   EVAL      VABDTS = datcor
030300140903     C                   EVAL      VABHMS = oracor
030400140903     C                   EVAL      VABCNT = 1
030500140903     C*
030600071210     C* ......VABNSP/VATNSP
030700071210     C* NSP => Stacco un numeratore da AZNUM
030800160609     C                   clear                   TRUL33DS
030900160609     C                   eval      I33OPE = *zeros
031000160609     C                   eval      I33CNU = 302
031100160609     C                   eval      I33NUM = 1
031200160609     C                   movel     TRUL33DS      KPJBU
031300160609     C                   call      'TRUL33R'
031400160609     C                   parm                    KPJBA
031500160609     C                   movel     KPJBU         TRUL33DS
031600160609     C                   if        O33ERR = *zeros
031700160609     C                   z-add     O33NRF        VABNSP
031800160609     C                   z-add     O33NRF        VATNSP
031900160609     C                   else
032000160609     C                   Z-ADD     1             errore
032100160609     C                   EVAL      vinmsg = %trimr(vinmsg)
032200160609     C                             + ' ' + 'VABNSP VATNSP'
032300160609     C                   endif
032400160617     C* ......VABNAS
032500160617     C                   EVAL      VABNAS=%trim(%subst(vindta:66:10))
032600160617     C* ......VABRSD / VABRD2
032700160617     C                   MOVEL     *blanks       w80              80
032800160617     C                   EVAL      w80=%trim(%subst(vindta:86:80))
032900160617     C                   EVAL      VABRSD=%trim(%subst(w80:1:35))
033000160617     C                   EVAL      VABRD2=%trim(%subst(w80:35+1:35))
033100150611     C* ......VABIND
033200160617     C                   EVAL      VABIND=%trim(%subst(vindta:171:30))
033300160610     C* ......VABCAD
033400160617     C                   EVAL      VABCAD=%trim(%subst(vindta:166:5))
033500150611     C* ......VABLOD
033600160617     C                   EVAL      VABLOD=%trim(%subst(vindta:201:35))
033700150611     C* ......VABPRD
033800160617     C                   EVAL      VABPRD=%trim(%subst(vindta:236:2))
033900150611     C* ......VABPRD
034000150611     C* Reperisco la provincia dal CAP e dalla località
034100150611     C                   IF        VABPRD  = *blanks AND
034200150611     C                             VABLOD <> *blanks AND
034300150611     C                             VABCAD <> *blanks AND
034400150611     C                             VABNZD  = *blanks
034500150611     C                   CLEAR                   TISI95DS
034600150611     C                   EVAL      I95TCN = '3'
034700150611     C                   Z-ADD     datcor        I95DAT
034800150611     C                   EVAL      I95CAP = VABCAD
034900150611     C                   EVAL      I95LOC = VABLOD
035000150611     C                   CALL      'TISI95R'
035100150611     C                   PARM                    TISI95DS
035200150611     C                   EVAL      VABPRD = O95PRV
035300150611     C                   ENDIF
035400160617     C* ......VABRMA / VABCAS
035500160617     C                   IF        %subst(vindta:245:1) = 'X'
035600160617     C                   EVAL      FlgCAS = '1'
035700160617     C                   EVAL      VABRMA=%trim(%subst(vindta:266:10))
035800160617     C                   ELSE
035900160617     C                   EVAL      VABRMA=%trim(%subst(vindta:246:10))
036000160617     C                   ENDIF
036100160617     C* ......VABNOT / VABNT2
036200160617     C                   MOVEL     *blanks       w200            200
036300160617     C                   EVAL      w200=%trim(%subst(vindta:286:200))
036400160617     C                   EVAL      VABNOT=%trim(%subst(w200:1:35))
036500160617     C                   EVAL      VABNT2=%trim(%subst(w200:35+1:35))
036600160609     C* ......VABRMN
036700160617     C                   EVAL      PiStr=%trim(%subst(vindta:14:10))
036800160617     C                   EXSR      CHKNUM
036900160617     C                   IF        PiInt=*on
037000160617     C                   Z-ADD     PiVal         VABRMN
037100160617     C                   ELSE
037200160617     C                   Z-ADD     1             VABRMN
037300160617     C                   EVAL      vinmsg = %trimr(vinmsg)
037400160617     C                             + ' ' + 'VABRMN'
037500160617     C                   ENDIF
037600150611     C* ......VATNOT_B
037700160509     C***                EVAL      VATNOT=%trim(%subst(vindta:509:15))
037800160509     C***                IF        VATNOT <> *blanks
037900160509     C***                EVAL      VATCMR = VABCMR
038000160509     C***                EVAL      VATCNT = VABCNT
038100160509     C***                EVAL      VATTRC = 'B'
038200160509     C***                ADD       1             §CTROKVT
038300160509     C***                WRITE     EDIVAT00
038400160509     C***                ENDIF
038500160321     C* ......VATNOT_S
038600160509     C***                EVAL      VATNOT=%trim(%subst(vindta:509:15))
038700160509     C***                IF        VATNOT <> *blanks
038800160509     C***                EVAL      VATCMR = VABCMR
038900160509     C***                EVAL      VATCNT = VABCNT
039000160509     C***                EVAL      VATTRC = 'S'
039100160509     C***                ADD       1             §CTROKVT
039200160509     C***                WRITE     EDIVAT00
039300160509     C***                ENDIF
039400160321     C* ......VATNOT_IJ
039500160509     C***                IF        %trim(%subst(vindta:509:15)) = *blanks
039600160509     C***                EVAL      VATNOT=%trim(%subst(vindta:597:35))
039700160509     C***                IF        VATNOT <> *blanks
039800160509     C***                EVAL      VATCMR = VABCMR
039900160509     C***                EVAL      VATCNT = VABCNT
040000160509     C***                EVAL      VATTRC = 'I'
040100160509     C***                ADD       1             §CTROKVT
040200160509     C***                WRITE     EDIVAT00
040300160509     C***                ENDIF
040400160509     C***                EVAL      VATNOT=%trim(%subst(vindta:597+35:40-35))
040500160509     C***                IF        VATNOT <> *blanks
040600160509     C***                EVAL      VATCMR = VABCMR
040700160509     C***                EVAL      VATCNT = VABCNT
040800160509     C***                EVAL      VATTRC = 'J'
040900160509     C***                ADD       1             §CTROKVT
041000160509     C***                WRITE     EDIVAT00
041100160509     C***                ENDIF
041200160509     C***                ENDIF
041300140922     C* ......VATNOT_P
041400150611     C***                if        *in53
041500140922     C*
041600150611     C* Solo se valorizzato RMA
041700150611     C***                if        wRMA_1 <> *blanks
041800150611     C***                eval      pIn_MaskPDF =
041900150611     C***                                  %trim(%subst(vindta:1292:100))
042000150611     C***                call      'TIS7P2SER'
042100150611     C***                parm      'W'           pIn_Opz           1
042200150611     C***                parm                    tivlrds
042300150611     C***                parm                    EDIVABDS
042400150611     C***                parm      '10'          pIn_CdIdAz        2
042500150611     C***                parm                    pIn_MaskPDF      50
042600150611     C***                parm      *blanks       pOut_Esito        1
042700150611     C***                endif
042800150611     C***                endif
042900140117     C*
043000140117     C                   ENDIF
043100151119     C***
043200160617     C* ...tipo record 'R1' (dettagli articoli)
043300160617     C                   IF        %trim(%subst(vindta:1:2)) = 'R1'
043400161003     C                   Z-ADD     *zeros        wQta              9 3
043500161003     C                   Z-ADD     *zeros        wPesoKg           9 3
043600160617     C
043700160617     C* ......wQta
043800160617     C                   EVAL      PiStr=%trim(%subst(vindta:98:6))
043900160617     C                   EXSR      CHKNUM
044000160617     C                   IF        PiNum=*on
044100161003     C                   EVAL      PiVal = PiVal/1000                           * da grammi a Kg
044200160617     C                   Z-ADD     PiVal         wQta
044300160617     C                   ELSE
044400160617     C                   Z-ADD     *zeros        wQta
044500160617     C                   EVAL      vinmsg = %trimr(vinmsg)
044600160617     C                             + ' ' + 'VABPKB'
044700160617     C                   ENDIF
044800160617     C* ......wPesoKg
044900160617     C                   EVAL      PiStr=%trim(%subst(vindta:125:7))
045000160617     C                   EXSR      CHKNUM
045100160617     C                   IF        PiNum=*on
045200160617     C                   Z-ADD     PiVal         wPesoKg
045300160617     C                   ELSE
045400160617     C                   Z-ADD     *zeros        wPesoKg
045500160617     C                   EVAL      vinmsg = %trimr(vinmsg)
045600160617     C                             + ' ' + 'VABPKB'
045700160617     C                   ENDIF
045800160617     C* ......VABPKB
045900160617     C                   EVAL      VABPKB = VABPKB + (wQta * wPesoKg)
046000160617     C*
046100160617     C                   ENDIF
046200160617     C***
046300160617     C* ...tipo record 'S1' (dettagli segnacolli)
046400160617     C                   IF        %trim(%subst(vindta:1:2)) = 'S1'
046500160617     C* ......VABNCL
046600160617     C                   ADD       1             VABNCL
046700151119     C* ......VATNOT_E
046800160617     C                   EVAL      VATNOT=%trim(%subst(vindta:16:10))
046900151119     C                   IF        VATNOT <> *blanks
047000151119     C                   EVAL      VATCMR = VABCMR
047100151119     C                   EVAL      VATCNT = VABCNT
047200151119     C                   EVAL      VATTRC = 'E'
047300151119     C                   ADD       1             §CTROKVT
047400151119     C                   WRITE     EDIVAT00
047500151119     C                   ENDIF
047600151119     C*
047700151119     C                   ENDIF
047800140117     C***
047900010202     C*
048000000801     C* Ebbene...
048100000801     C                   ADD       1             §CTRMO
048200010201     C                   IF        errore <> *zeros
048300000801     C                   ADD       1             §CTRNO
048400000801     C                   EVAL      vinflg = '2'
048500000801     C                   ELSE
048600010201     C                   ADD       1             §CTROKVB
048700000801     C                   ENDIF
048800000801     C*
048900000801     C                   ENDSR
049000010202     C*----------------------------------------------------*
049100140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049200010202     C*----------------------------------------------------*
049300020305     C     PREVAT        BEGSR
049400010202     C*
049500140117     C* Compongo il nome del membro da dare al EDIVATWR
049600010202     C                   eval      parmbr = vlrhdl
049700010202     C                   movel     'M'           parmbr
049800060113     C                   eval      parccm = vlrksc
049900010202     C                   eval      paropz = '1'
050000010202     C* Effettuo la chiamata al CLLE preposto
050100140117     C                   call(e)   'TITVEVTC'
050200010202     C                   parm                    parccm
050300010202     C                   parm                    parmbr
050400010202     C                   parm                    paropz
050500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600010202     C                   if        %error
050700010202     C                   movel     '1'           chkcall
050800010202     C                   else
050900010202     C                   movel     '0'           chkcall
051000010202     C                   endif
051100010202     C*
051200010202     C                   ENDSR
051300000801     C*----------------------------------------------------*
051400000801     C*  CONTROLLO NUMERICITA' CAMPI
051500000801     C*----------------------------------------------------*
051600000801     C     CHKNUM        BEGSR
051700000801     C*
051800000801     C                   call(e)   'ISNUMERIC'
051900000801     C                   PARM                    PiStr            30
052000160617     C                   PARM      '.'           PiDecChr          1
052100000801     C                   PARM      *ZEROS        PiVal            30 9
052200000801     C                   PARM      '0'           PiInt             1
052300000801     C                   PARM      '0'           PiNum             1
052400000801     C                   IF        %error
052500000801     C                   EVAL      PiInt=*off
052600000801     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900000801     C***
053000000801
053100011113
053200011113     C*----------------------------------------------------*
053300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053400011113     C*----------------------------------------------------*
053500011113     C     CHKIMPDIV     BEGSR
053600011113     C*
053700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053800011113     C                   Z-ADD     *zeros        wrkDec            9 9
053900011113     C*
054000011113     C* Come prima cosa effettuo considerazioni sulla divisa
054100011113     C                   IF        vabIAS > *zeros
054200011113     C                   IF        vabVAS <> 'EUR'
054300011113     C                   EVAL      vabVAS =  'ITL'
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   IF        vabCAS > *zeros
054800011113     C                   IF        vabVCA <> 'EUR'
054900011113     C                   EVAL      vabVCA =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabVMD > *zeros
055400020305     C                   IF        vabVAD <> 'EUR'
055500011113     C                   EVAL      vabVAD =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056000011113     C                   Z-ADD     vabIAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAS = 'ITL'
056300011113     C                   EVAL      vabIAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056800011113     C                   Z-ADD     vabCAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVCA = 'ITL'
057100011113     C                   EVAL      vabCAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057600011113     C                   Z-ADD     vabVMD        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVAD = 'ITL'
057900011113     C                   EVAL      vabVMD = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   ENDSR
058400011113     C***
058500011113
058600011113
058700000801
058800000801
058900990920      /TITLE Invio dei dati al punto operativo.
059000010202     C     invio         BEGSR
059100990920     C*
059200140117     C* 1° invio EDIVAT
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500140117     C                   eval      cmzfld = 'EDIVATWR'
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900140117     C                   eval      cmzfla = 'EDIVAT0F'
060000140117     C                   eval      cmzmba = 'EDIVAT0F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600020305     C                   move      §ctrokvt      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010205     C                   if        %error
061200010205     C                             or cmzerr = '1'
061300010205     C                             or esito  = '1'
061400010205     C                   eval      wrkesito = '3'
061500010205     C                   else
061600010201     C*
061700140117     C* 2° invio EDIVAB
061800010201     C                   reset                   dscmz
061900010201     C                   move      vlrpoi        cmzdst
062000010201     C                   eval      cmzfld = vlrfou
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400140117     C                   eval      cmzfla = 'EDIVAB0F'
062500140117     C                   eval      cmzmba = 'EDIVAB0F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100010201     C                   move      §ctrokvb      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010201     C                   if        %error
063700010201     C                             or cmzerr = '1'
063800010201     C                             or esito  = '1'
063900010201     C                   eval      wrkesito = '3'
064000010201     C                   endif
064100010205     C                   endif
064200990920     C*
064300000613     C                   ENDSR
064400000613     C***
064500070411
064600070411     C     *pssr         BEGSR
064700070411     C*
064800070411     C                   if        %open(tivin00r)
064900070411     C                   close     tivin00r
065000070411     C                   endif
065100140117     C                   if        %open(edivabwr)
065200140117     C                   close     edivabwr
065300070411     C                   endif
065400140117     C                   if        %open(edivatwr)
065500140117     C                   close     edivatwr
065600070411     C                   endif
065700070411     C*
065800070411     C* Effettuo la chiamata al CLLE preposto
065900140117     C                   call(e)   'TITVEVTC'
066000070411     C                   parm                    parccm
066100070411     C                   parm                    parmbr
066200070411     C                   parm      '2'           paropz
066300070411     C*
066400070411     C                   eval      wrkesito = '2'
066500070411     C*
066600070411     C                   seton                                        LR
066700070411     C*
066800070411     C                   ENDSR     '*CANCL'
066900070411     C***
067000070411
067100990910
067200000613     C     *inzsr        BEGSR
067300990910     C*
067400990910     C     *entry        plist
067500990920     C                   parm                    tivlrds
067600990921     C                   parm      wrkesito      esito
067700000724     C                   parm                    prmlit
067800000710     C                   parm                    prmfir
067900000613     C*
068000000830     C* CALCOLA LA DATA CORRENTE
068100140117     C                   time                    wn14             14 0
068200140117     C                   movel     wn14          oracor            6 0          *ORA
068300100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068400100722     C                   eval      datcor = %dec(%date() : *ISO)
068500000830     C*
068600000613     C                   ENDSR
068700000613     C***
