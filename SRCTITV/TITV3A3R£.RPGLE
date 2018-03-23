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
004200170705     C*
004300170705     C* Avvio monitoring
004400170705     C                   monitor
004500000613     C*
004600150213     C                   EXSR      DEFCAM
004700150213     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004800070719     C*
004900070719     C* Esegue lancio TISI95R solo x chiusura
005000070719     C                   CLEAR                   TISI95DS
005100070719     C                   EVAL      I95TLA = 'C'
005200070719     C                   CALL      'TISI95R'
005300070719     C                   PARM                    TISI95DS
005400000613     C*
005500010202     C* Effettuo la chiamata al CLLE preposto
005600140922     C                   call(e)   'TITVEVTC'
005700140922     C                   parm                    parccm
005800140922     C                   parm                    parmbr
005900140922     C                   parm      '2'           paropz
006000170731     C*
006100170731     C                   if        *in53
006200170731     C                   call      'TIS7P2SER'
006300170731     C                   parm      'C'           pIn_Opz           1
006400170731     C                   parm                    tivlrds
006500170731     C                   parm                    EDIVABDS
006600170731     C                   parm      *blanks       pIn_CdIdAz        2
006700170731     C                   parm      *blanks       pIn_MaskPDF      50
006800170731     C                   parm      *blanks       pOut_Esito        1
006900170731     C                   endif
007000170705     C*
007100170705     C* Gestisco errore monitoring
007200170705     C                   on-error
007300170705     C                   exsr      exeERR
007400170705     C*
007500170705     C* Stoppo il monitoring
007600170705     C                   endmon
007700000616     C*
007800010201     C                   seton                                        LR
007900990908
008000000801
008100910830     C*--------------------------------------------------------
008200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008300910830     C*--------------------------------------------------------
008400040526     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900140117     C                   if        not %open(edivabwr)
009000140117     C                   open      edivabwr
009100990914     C                   endif
009200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009300020305     C                   exsr      prevat
009400010201     C*
009500010202     C                   if        chkcall = '0'
009600010202     C*
009700140117     C                   if        not %open(edivatwr)
009800140117     C                   open      edivatwr
009900010201     C                   endif
010000990910     C*
010100010201     C                   clear                   §CTROKVB          5 0
010200020305     C                   clear                   §CTROKVT          5 0
010300000801     C                   clear                   §CTRMO            5 0
010400000801     C                   clear                   §CTRNO            5 0
010500100729     C*
010600100729     C                   z-add     1             wGiro             1 0
010700140117     C*
010800140902     C                   if        *in53
010900140117     C                   call      'TIS7P2SER'
011000140117     C                   parm      'O'           pIn_Opz           1
011100140117     C                   parm                    tivlrds
011200140117     C                   parm                    EDIVABDS
011300140117     C                   parm      *blanks       pIn_CdIdAz        2
011400140117     C                   parm      *blanks       pIn_MaskPDF      50
011500140117     C                   parm      *blanks       pOut_Esito        1
011600140902     C                   endif
011700130708     C*
011800161003     C* ......inizializzazioni iniziali e formati record file Bartolini
011900161003     C                   EXSR      INZVAR
012000161003     C                   EXSR      DEFCAM
012100161003     C*
012200130708     C                   SETOFF                                       31
012300921023     C                   DO        *HIVAL
012400990913     C*
012500990915     C                   READ      tivin00r                               70
012600040910     C                   if        vindta > *blanks
012700000613     C                   add       1             rrnum
012800000801     C*
012900000801     C                   if        *in70 = *off
013000000801     C                             and
013100000801     C                             (vinflg = *blanks
013200000801     C                              or vinflg = '0'
013300000801     C                              or vinflg = '2')
013400000801     C*
013500000801     C                   clear                   vinmsg
013600000801     C                   eval      vinflg = '1'
013700040910     C*
013800040910     C* Eseguo routine d traduzione
013900040910     C                   exsr      impvabvat
014000040802     C*
014100010305     C                   endif
014200000905     C*
014300000905     C                   else
014400000905     C                   eval      vinflg = '1'
014500000905     C                   endif
014600000905     C*
014700000905     C  N70              update    tivin000
014800000905     C*
014900991022     C  N70              ENDdo
015000100722     C*
015100100722     C* Scarico i buffer testata ancora "in canna"
015200140922     C*
015300140922     C  N31              exsr      CHKWRI
015400140117     C  N31              WRITE     EDIVAB00
015500130708     C                   SETOFF                                       31
015600010202     C*
015700010202     C                   endif
015800990910
015900990910     C* Se non ci sono record con errori ...
016000000710     C                   if        §ctrno = 0
016100990910     C* ... restituisco esito OK.
016200990921     C                   eval      wrkesito = '0'
016300990910     C                   else
016400010201     C                   if        §ctrokvb > 0
016500990921     C                   eval      wrkesito = '1'
016600000710     C                   else
016700000710     C                   eval      wrkesito = '2'
016800990910     C                   endif
016900000710     C                   endif
017000990910     C*
017100990914     C                   if        %open(tivin00r)
017200990908     C                   close     tivin00r
017300990914     C                   endif
017400140117     C                   if        %open(edivabwr)
017500140117     C                   close     edivabwr
017600990914     C                   endif
017700140117     C                   if        %open(edivatwr)
017800140117     C                   close     edivatwr
017900010201     C                   endif
018000990910     C*
018100010201     C                   if        §ctrokvb > 0
018200000724     C                             and vlrpoi <> *zeros
018300010202     C                   exsr      invio
018400990920     C                   endif
018500990920     C*
018600910830     C                   ENDSR
018700000613     C***
018800140922
018900140922     C*----------------------------------------------------*
019000140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
019100140922     C*----------------------------------------------------*
019200140922     C     CHKWRI        BEGSR
019300140922     C*
019400140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
019500140922     C                   IF        FlgCAS <> '0'
019600140922     C                   IF        VABCBO = '1'
019700140922     C                   EVAL      VABCBO = '4'
019800140922     C                   ELSE
019900140922     C                   EVAL      VABCBO = '6'
020000140922     C                   ENDIF
020100140922     C                   ENDIF
020200140922     C*
020300140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
020400140922     C                   EXSR      CHKIMPDIV
020500140922     C*
020600140922     C                   ENDSR
020700990920
020800000801     C*----------------------------------------------------*
020900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021000000801     C*----------------------------------------------------*
021100010201     C     INZVAR        BEGSR
021200000801     C*
021300040802     C                   Z-ADD     *zeros        Num5_0            5 0
021400040802     C                   MOVEL     '0'           FlgCAS            1
021500000801     C*
021600000801     C                   ENDSR
021700000801     C*----------------------------------------------------*
021800040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021900000801     C*----------------------------------------------------*
022000000801     C     DEFCAM        BEGSR
022100000801     C*
022200140117     C                   CLEAR                   EDIVAB00
022300140117     C                   CLEAR                   EDIVAT00
022400020619     C* Imposto i valori di default...
022500170705     C                   Z-ADD     1111565       VABCCM
022600170705     C                   Z-ADD     1111565       VATCCM
022700161003     C                   Z-ADD     111           VABLNP
022800161003     C                   Z-ADD     111           VATLNP
022900170705     C                   Z-ADD     001           VABCTR
023000151119     C                   MOVEL     '7Q'          VABCTM
023100040823     C                   MOVEL     '1'           VABCBO
023200020619     C* ... e poi verifico se sono stati passati come parametri
023300020619     C                   IF        vlrppt > *blanks
023400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
023500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023600020619     C                   EXSR      CHKNUM
023700020619     C                   IF        PiInt=*on
023800020619     C                   Z-ADD     PiVal         VABCCM
023900020619     C                   Z-ADD     PiVal         VATCCM
024000020619     C                   ENDIF
024100040506     C                   ENDIF
024200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
024300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024400020619     C                   EXSR      CHKNUM
024500020619     C                   IF        PiInt=*on
024600020619     C                   Z-ADD     PiVal         VABLNP
024700020619     C                   Z-ADD     PiVal         VATLNP
024800040506     C                   ENDIF
024900020619     C                   ENDIF
025000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
025100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025200020619     C                   EXSR      CHKNUM
025300020619     C                   IF        PiInt=*on
025400020619     C                   Z-ADD     PiVal         VABCTR
025500040506     C                   ENDIF
025600020619     C                   ENDIF
025700060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
025800060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
025900060202     C                   ENDIF
026000140902     C                   IF        %subst(vlrppt:16:1) = 'S'
026100140902     C                   SETON                                        53
026200140902     C                   ELSE
026300140902     C                   SETOFF                                       53
026400140902     C                   ENDIF
026500020619     C                   ENDIF
026600000801     C*
026700000801     C                   ENDSR
026800000801     C*----------------------------------------------------*
026900140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
027000000801     C*----------------------------------------------------*
027100040910     C     IMPVABVAT     BEGSR
027200040910     C*
027300040910     C* Traduzione relativa ai tipi record del file del cliente
027400040910     C*
027500071210     C*
027600071210     C***
027700151119     C                   MOVEL     *blanks       savCMR           35
027800170705     C                   EVAL      savCMR = 'UNILEVER  INNOCENTI '+
027900161003     C                                      %trim(%char(%date()))
028000140922     C***
028100170705     C* ...tipo record 'T2' (testata)
028200170705     C                   IF        %trim(%subst(vindta:1:2)) = 'T2'
028300100722     C*
028400100722     C* Se nn primo giro => scarico il buffer precedente
028500150611     C                   if        wGiro = 1
028600100722     C                   eval      wGiro = 2
028700100722     C                   else
028800140922     C*
028900140922     C  N31              exsr      CHKWRI
029000140117     C  N31              WRITE     EDIVAB00
029100130708     C                   SETOFF                                       31
029200100722     C                   endif
029300100722     C*
029400071210     C* Resetto indicatore d anomalia sul singolo record
029500071210     C                   eval      vinflg = '1'
029600071210     C* ......inizializzazioni iniziali e formati record file Bartolini
029700071210     C                   EXSR      INZVAR
029800071210     C                   EXSR      DEFCAM
029900071210     C*
030000071210     C                   Z-ADD     *zeros        errore            1 0
030100071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030200071210     C                   MOVEL     datcor        VABAAS
030300071210     C                   MOVEL     datcor        VATAAS
030400071210     C                   MOVE      datcor        VABMGS
030500071210     C                   MOVE(P)   vlrpoi        VABFGS
030600071210     C                   MOVE(P)   vlrpoi        VATFGS
030700140903     C*
030800140903     C* Dai "fissi"
030900140903     C                   EVAL      VABCMR = savCMR
031000140903     C                   EVAL      VABDCM = datcor
031100140903     C                   EVAL      VABDTS = datcor
031200140903     C                   EVAL      VABHMS = oracor
031300140903     C                   EVAL      VABCNT = 1
031400140903     C*
031500071210     C* ......VABNSP/VATNSP
031600071210     C* NSP => Stacco un numeratore da AZNUM
031700160609     C                   clear                   TRUL33DS
031800160609     C                   eval      I33OPE = *zeros
031900160609     C                   eval      I33CNU = 302
032000160609     C                   eval      I33NUM = 1
032100160609     C                   movel     TRUL33DS      KPJBU
032200160609     C                   call      'TRUL33R'
032300160609     C                   parm                    KPJBA
032400160609     C                   movel     KPJBU         TRUL33DS
032500160609     C                   if        O33ERR = *zeros
032600160609     C                   z-add     O33NRF        VABNSP
032700160609     C                   z-add     O33NRF        VATNSP
032800160609     C                   else
032900160609     C                   Z-ADD     1             errore
033000160609     C                   EVAL      vinmsg = %trimr(vinmsg)
033100160609     C                             + ' ' + 'VABNSP VATNSP'
033200160609     C                   endif
033300160617     C* ......VABNAS
033400170705     C                   EVAL      VABNAS=%trim(%subst(vindta:261:10))
033500160617     C* ......VABRSD / VABRD2
033600170705     C                   MOVEL     *blanks       w80              80
033700170705     C                   EVAL      w80=%trim(%subst(vindta:70:80))
033800170719     C                   EVAL      VABRSD=%trim(%subst(w80:1:35))
033900160617     C                   EVAL      VABRD2=%trim(%subst(w80:35+1:35))
034000150611     C* ......VABIND
034100170705     C                   EVAL      VABIND=%trim(%subst(vindta:155:30))
034200160610     C* ......VABCAD
034300170705     C                   EVAL      VABCAD=%trim(%subst(vindta:150:5))
034400150611     C* ......VABLOD
034500170705     C                   EVAL      VABLOD=%trim(%subst(vindta:185:35))
034600170705     C* ......VABNZD
034700170705     C                   EVAL      VABNZD=%trim(%subst(vindta:224:2))
034800170705     C                   IF        VABNZD='IT'
034900170705     C                   EVAL      VABNZD=*blanks
035000170705     C                   ENDIF
035100150611     C* ......VABPRD
035200150611     C* Reperisco la provincia dal CAP e dalla località
035300150611     C                   IF        VABPRD  = *blanks AND
035400150611     C                             VABLOD <> *blanks AND
035500150611     C                             VABCAD <> *blanks AND
035600150611     C                             VABNZD  = *blanks
035700150611     C                   CLEAR                   TISI95DS
035800150611     C                   EVAL      I95TCN = '3'
035900150611     C                   Z-ADD     datcor        I95DAT
036000150611     C                   EVAL      I95CAP = VABCAD
036100150611     C                   EVAL      I95LOC = VABLOD
036200150611     C                   CALL      'TISI95R'
036300150611     C                   PARM                    TISI95DS
036400150611     C                   EVAL      VABPRD = O95PRV
036500150611     C                   ENDIF
036600170705     C* ......VABRMA
036700170705     C                   EVAL      VABRMA=%trim(%subst(vindta:20:10))
036800170705     C* ......VABCAS
036900170705     C                   IF        %subst(vindta:240:1) = 'X'
037000160617     C                   EVAL      FlgCAS = '1'
037100170705     C                   EVAL      VABTIC='BM'
037200170719     C                   EVAL      VABVCA='EUR'
037300160617     C                   ENDIF
037400160617     C* ......VABNOT / VABNT2
037500160617     C                   MOVEL     *blanks       w200            200
037600170705     C                   EVAL      w200=%trim(%subst(vindta:292:200))
037700160617     C                   EVAL      VABNOT=%trim(%subst(w200:1:35))
037800160617     C                   EVAL      VABNT2=%trim(%subst(w200:35+1:35))
037900160609     C* ......VABRMN
038000170705     C                   EVAL      PiStr=%trim(%subst(vindta:10:10))
038100160617     C                   EXSR      CHKNUM
038200160617     C                   IF        PiInt=*on
038300160617     C                   Z-ADD     PiVal         VABRMN
038400160617     C                   ELSE
038500160617     C                   Z-ADD     1             VABRMN
038600160617     C                   EVAL      vinmsg = %trimr(vinmsg)
038700160617     C                             + ' ' + 'VABRMN'
038800160617     C                   ENDIF
038900170705     C* ......VABDCR
039000170705     C                   IF        %subst(vindta:30:10) <> *blanks
039100170705     C                   EVAL      PiStr=%trim(%subst(vindta:30:10))
039200170705     C                   EXSR      CHKNUM
039300170705     C                   IF        PiInt=*on
039400170705     C                   Z-ADD     PiVal         VABDCR
039500170705     C                   ELSE
039600170705     C                   EVAL      vinmsg = %trimr(vinmsg)
039700170705     C                             + ' ' + 'VABDCR'
039800170705     C                   ENDIF
039900170705     C                   ENDIF
040000170705     C* ......VABNCL
040100170705     C                   EVAL      PiStr=%trim(%subst(vindta:226:3))
040200170705     C                   EXSR      CHKNUM
040300170705     C                   IF        PiInt=*on
040400170705     C                   Z-ADD     PiVal         VABNCL
040500170705     C                   ELSE
040600170705     C                   EVAL      vinmsg = %trimr(vinmsg)
040700170705     C                             + ' ' + 'VABNCL'
040800170705     C                   ENDIF
040900170705     C* ......VABPKB
041000170705     C                   EVAL      PiStr=%trim(%subst(vindta:229:11))
041100170705     C                   EXSR      CHKNUM
041200170705     C                   IF        PiNum=*on
041300170705     C                   Z-ADD     PiVal         VABPKB
041400170705     C                   ELSE
041500170705     C                   EVAL      vinmsg = %trimr(vinmsg)
041600170705     C                             + ' ' + 'VABPKB'
041700170705     C                   ENDIF
041800170705     C* ......VABCAS
041900170705     C                   IF        FlgCAS = '1'
042000170705     C                   EVAL      PiStr=%trim(%subst(vindta:271:11))
042100170705     C                   EXSR      CHKNUM
042200170705     C                   IF        PiNum=*on
042300170705     C                   Z-ADD     PiVal         VABCAS
042400170705     C                   ELSE
042500170705     C                   EVAL      vinmsg = %trimr(vinmsg)
042600170705     C                             + ' ' + 'VABCAS'
042700170705     C                   ENDIF
042800170705     C                   ENDIF
042900150611     C* ......VATNOT_B
043000171012     C                   EVAL      VATNOT=%trim(%subst(vindta:519:15))
043100171012     C                   IF        VATNOT <> *blanks
043200171012     C                   EVAL      VATCMR = VABCMR
043300171012     C                   EVAL      VATCNT = VABCNT
043400171012     C                   EVAL      VATTRC = 'B'
043500171012     C                   ADD       1             §CTROKVT
043600171012     C                   WRITE     EDIVAT00
043700171012     C                   ENDIF
043800160321     C* ......VATNOT_S
043900171019     C                   EVAL      VATNOT=%trim(%subst(vindta:519:15))
044000171019     C                   IF        VATNOT <> *blanks
044100171019     C* i 2 flag devono essere ' N' (abilitazione SMS/alert come da std, no l'email perché non viene
044200171019     C*  passata)
044300171019     C                   EVAL      %subst(VATNOT:17:1)=' '
044400171019     C                   EVAL      %subst(VATNOT:18:1)='N'
044500171019     C                   EVAL      VATCMR = VABCMR
044600171019     C                   EVAL      VATCNT = VABCNT
044700171019     C                   EVAL      VATTRC = 'S'
044800171019     C                   ADD       1             §CTROKVT
044900171019     C                   WRITE     EDIVAT00
045000171019     C                   ENDIF
045100160321     C* ......VATNOT_IJ
045200160509     C***                IF        %trim(%subst(vindta:509:15)) = *blanks
045300160509     C***                EVAL      VATNOT=%trim(%subst(vindta:597:35))
045400160509     C***                IF        VATNOT <> *blanks
045500160509     C***                EVAL      VATCMR = VABCMR
045600160509     C***                EVAL      VATCNT = VABCNT
045700160509     C***                EVAL      VATTRC = 'I'
045800160509     C***                ADD       1             §CTROKVT
045900160509     C***                WRITE     EDIVAT00
046000160509     C***                ENDIF
046100160509     C***                EVAL      VATNOT=%trim(%subst(vindta:597+35:40-35))
046200160509     C***                IF        VATNOT <> *blanks
046300160509     C***                EVAL      VATCMR = VABCMR
046400160509     C***                EVAL      VATCNT = VABCNT
046500160509     C***                EVAL      VATTRC = 'J'
046600160509     C***                ADD       1             §CTROKVT
046700160509     C***                WRITE     EDIVAT00
046800160509     C***                ENDIF
046900160509     C***                ENDIF
047000140922     C* ......VATNOT_P
047100150611     C***                if        *in53
047200140922     C*
047300150611     C* Solo se valorizzato RMA
047400150611     C***                if        wRMA_1 <> *blanks
047500150611     C***                eval      pIn_MaskPDF =
047600150611     C***                                  %trim(%subst(vindta:1292:100))
047700150611     C***                call      'TIS7P2SER'
047800150611     C***                parm      'W'           pIn_Opz           1
047900150611     C***                parm                    tivlrds
048000150611     C***                parm                    EDIVABDS
048100150611     C***                parm      '10'          pIn_CdIdAz        2
048200150611     C***                parm                    pIn_MaskPDF      50
048300150611     C***                parm      *blanks       pOut_Esito        1
048400150611     C***                endif
048500150611     C***                endif
048600140117     C*
048700140117     C                   ENDIF
048800160617     C***
048900170705     C* ...tipo record 'S2' (dettagli segnacolli)
049000170705     C                   IF        %trim(%subst(vindta:1:2)) = 'S2'
049100151119     C* ......VATNOT_E
049200170705     C                   EVAL      VATNOT=%trim(%subst(vindta:6:14))
049300151119     C                   IF        VATNOT <> *blanks
049400151119     C                   EVAL      VATCMR = VABCMR
049500151119     C                   EVAL      VATCNT = VABCNT
049600151119     C                   EVAL      VATTRC = 'E'
049700151119     C                   ADD       1             §CTROKVT
049800151119     C                   WRITE     EDIVAT00
049900151119     C                   ENDIF
050000151119     C*
050100151119     C                   ENDIF
050200140117     C***
050300010202     C*
050400000801     C* Ebbene...
050500000801     C                   ADD       1             §CTRMO
050600010201     C                   IF        errore <> *zeros
050700000801     C                   ADD       1             §CTRNO
050800000801     C                   EVAL      vinflg = '2'
050900000801     C                   ELSE
051000010201     C                   ADD       1             §CTROKVB
051100000801     C                   ENDIF
051200000801     C*
051300000801     C                   ENDSR
051400010202     C*----------------------------------------------------*
051500140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051600010202     C*----------------------------------------------------*
051700020305     C     PREVAT        BEGSR
051800010202     C*
051900140117     C* Compongo il nome del membro da dare al EDIVATWR
052000010202     C                   eval      parmbr = vlrhdl
052100010202     C                   movel     'M'           parmbr
052200060113     C                   eval      parccm = vlrksc
052300010202     C                   eval      paropz = '1'
052400010202     C* Effettuo la chiamata al CLLE preposto
052500140117     C                   call(e)   'TITVEVTC'
052600010202     C                   parm                    parccm
052700010202     C                   parm                    parmbr
052800010202     C                   parm                    paropz
052900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053000010202     C                   if        %error
053100010202     C                   movel     '1'           chkcall
053200010202     C                   else
053300010202     C                   movel     '0'           chkcall
053400010202     C                   endif
053500010202     C*
053600010202     C                   ENDSR
053700000801     C*----------------------------------------------------*
053800000801     C*  CONTROLLO NUMERICITA' CAMPI
053900000801     C*----------------------------------------------------*
054000000801     C     CHKNUM        BEGSR
054100000801     C*
054200000801     C                   call(e)   'ISNUMERIC'
054300000801     C                   PARM                    PiStr            30
054400170705     C                   PARM      ','           PiDecChr          1
054500000801     C                   PARM      *ZEROS        PiVal            30 9
054600000801     C                   PARM      '0'           PiInt             1
054700000801     C                   PARM      '0'           PiNum             1
054800000801     C                   IF        %error
054900000801     C                   EVAL      PiInt=*off
055000000801     C                   ENDIF
055100000801     C*
055200000801     C                   ENDSR
055300000801     C***
055400000801
055500011113
055600011113     C*----------------------------------------------------*
055700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055800011113     C*----------------------------------------------------*
055900011113     C     CHKIMPDIV     BEGSR
056000011113     C*
056100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056200011113     C                   Z-ADD     *zeros        wrkDec            9 9
056300011113     C*
056400011113     C* Come prima cosa effettuo considerazioni sulla divisa
056500011113     C                   IF        vabIAS > *zeros
056600011113     C                   IF        vabVAS <> 'EUR'
056700011113     C                   EVAL      vabVAS =  'ITL'
056800011113     C                   ENDIF
056900011113     C                   ENDIF
057000011113     C*
057100011113     C                   IF        vabCAS > *zeros
057200011113     C                   IF        vabVCA <> 'EUR'
057300011113     C                   EVAL      vabVCA =  'ITL'
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C                   IF        vabVMD > *zeros
057800020305     C                   IF        vabVAD <> 'EUR'
057900011113     C                   EVAL      vabVAD =  'ITL'
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058400011113     C                   Z-ADD     vabIAS        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVAS = 'ITL'
058700011113     C                   EVAL      vabIAS = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059200011113     C                   Z-ADD     vabCAS        wrkDec
059300011113     C                   IF        wrkDec > *zeros
059400011113     C                   IF        vabVCA = 'ITL'
059500011113     C                   EVAL      vabCAS = *zeros
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060000011113     C                   Z-ADD     vabVMD        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVAD = 'ITL'
060300011113     C                   EVAL      vabVMD = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C                   ENDSR
060800011113     C***
060900011113
061000011113
061100000801
061200000801
061300990920      /TITLE Invio dei dati al punto operativo.
061400010202     C     invio         BEGSR
061500990920     C*
061600140117     C* 1° invio EDIVAT
061700010201     C                   reset                   dscmz
061800010201     C                   move      vlrpoi        cmzdst
061900140117     C                   eval      cmzfld = 'EDIVATWR'
062000010201     C                   eval      cmzmbd = vlrhdl
062100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062200021009     C***                if        prmfir = *blanks
062300140117     C                   eval      cmzfla = 'EDIVAT0F'
062400140117     C                   eval      cmzmba = 'EDIVAT0F'
062500021009     C***                else
062600021009     C***                eval      cmzfla = prmfir
062700021009     C***                eval      cmzmba = prmfir
062800021009     C***                endif
062900010201     C                   eval      cmznrr = *zeros
063000020305     C                   move      §ctrokvt      cmznrr
063100021018     C                   eval      cmzlba = vlrfl1
063200010201     C                   call(e)   'TIS711C'
063300010201     C                   parm                    dscmz
063400010201     C                   parm      *blanks       esito
063500010205     C                   if        %error
063600010205     C                             or cmzerr = '1'
063700010205     C                             or esito  = '1'
063800010205     C                   eval      wrkesito = '3'
063900010205     C                   else
064000010201     C*
064100140117     C* 2° invio EDIVAB
064200010201     C                   reset                   dscmz
064300010201     C                   move      vlrpoi        cmzdst
064400010201     C                   eval      cmzfld = vlrfou
064500010201     C                   eval      cmzmbd = vlrhdl
064600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064700021009     C***                if        prmfir = *blanks
064800140117     C                   eval      cmzfla = 'EDIVAB0F'
064900140117     C                   eval      cmzmba = 'EDIVAB0F'
065000021009     C***                else
065100021009     C***                eval      cmzfla = prmfir
065200021009     C***                eval      cmzmba = prmfir
065300021009     C***                endif
065400010201     C                   eval      cmznrr = *zeros
065500010201     C                   move      §ctrokvb      cmznrr
065600021018     C                   eval      cmzlba = vlrfl1
065700010201     C                   call(e)   'TIS711C'
065800010201     C                   parm                    dscmz
065900010201     C                   parm      *blanks       esito
066000010201     C                   if        %error
066100010201     C                             or cmzerr = '1'
066200010201     C                             or esito  = '1'
066300010201     C                   eval      wrkesito = '3'
066400010201     C                   endif
066500010205     C                   endif
066600990920     C*
066700000613     C                   ENDSR
066800000613     C***
066900070411
067000170705     C     exeERR        BEGSR
067100070411     C*
067200070411     C                   if        %open(tivin00r)
067300070411     C                   close     tivin00r
067400070411     C                   endif
067500140117     C                   if        %open(edivabwr)
067600140117     C                   close     edivabwr
067700070411     C                   endif
067800140117     C                   if        %open(edivatwr)
067900140117     C                   close     edivatwr
068000070411     C                   endif
068100070411     C*
068200070411     C* Effettuo la chiamata al CLLE preposto
068300140117     C                   call(e)   'TITVEVTC'
068400070411     C                   parm                    parccm
068500070411     C                   parm                    parmbr
068600070411     C                   parm      '2'           paropz
068700070411     C*
068800070411     C                   eval      wrkesito = '2'
068900070411     C*
069000170705     C                   dump(A)
069100070411     C                   seton                                        LR
069200070411     C*
069300170705     C                   ENDSR
069400070411     C***
069500070411
069600990910
069700000613     C     *inzsr        BEGSR
069800990910     C*
069900990910     C     *entry        plist
070000990920     C                   parm                    tivlrds
070100990921     C                   parm      wrkesito      esito
070200000724     C                   parm                    prmlit
070300000710     C                   parm                    prmfir
070400000613     C*
070500000830     C* CALCOLA LA DATA CORRENTE
070600140117     C                   time                    wn14             14 0
070700140117     C                   movel     wn14          oracor            6 0          *ORA
070800100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070900100722     C                   eval      datcor = %dec(%date() : *ISO)
071000000830     C*
071100000613     C                   ENDSR
071200000613     C***
