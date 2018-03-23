000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
002600141028     D wTag            s            128    VARYING INZ
002700141028     D wRecErr         s              1  0 INZ(*zeros)
002800141028     D wFlgCAS         s              1    INZ(*blanks)
002900141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003000141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003100141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003200141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003300141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
003700141028
003800141028     D*------------------
003900141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004000141028     D*------------------
004100141028     D CharNUM         S              1
004200141028     D CharSOS         S              1
004300141028     D posDaDft        S              3  0 INZ(*zeros)
004400141028     D posADft         S              3  0 INZ(*zeros)
004500141028     D wGiroDft        s              1  0 INZ(*zeros)
004600141028
004700141028
004800141028     D*------------------
004900141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005000141028     D*------------------
005100141028     D InStringa       S          65535A   VARYING                              (stringa input)
005200141028     D InSepara        S             10A                                        (separatore)
005300141028     D InTypeOut       S              1A                                        (tipo output)
005400141028     D wSkParam        S          65535A                                        (output)
005500141028     D OutErrore       S              1A                                        (flag errore)
005600141028     D OutMsg          S             80A                                        (messaggio errore)
005700141028     D SkSplitCSV_5    S            256    DIM(256)
005800141028
005900000830
006000041025     D*------------------
006100041025     D* DS REPERIMENTO NUMERATORE
006200041025     D*------------------
006300041025     D trul33ds      e ds                  inz
006400041025     D*------------------
006500041025     D* DS ARCHITETTURA
006600041025     D*------------------
006700041025     D kpjba         e ds                  inz
006800041025     D*------------------
006900990908
007000141028
007100141028     D*------------------
007200141028     D* LINKING A DEFINIZIONI ESTERNE
007300141028     D*------------------
007400141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007500141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007600141028
007700141028
007800010201
007900010201
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008500070719     C*
008600070719     C* Esegue lancio TISI95R solo x chiusura
008700070719     C                   CLEAR                   TISI95DS
008800070719     C                   EVAL      I95TLA = 'C'
008900070719     C                   CALL      'TISI95R'
009000070719     C                   PARM                    TISI95DS
009100000613     C*
009200010202     C* Effettuo la chiamata al CLLE preposto
009300140922     C                   call(e)   'TITVEVTC'
009400140922     C                   parm                    parccm
009500140922     C                   parm                    parmbr
009600140922     C                   parm      '2'           paropz
009700170731     C*
009800170731     C                   if        *in53
009900170731     C                   call      'TIS7P2SER'
010000170731     C                   parm      'C'           pIn_Opz           1
010100170731     C                   parm                    tivlrds
010200170731     C                   parm                    EDIVABDS
010300170731     C                   parm      *blanks       pIn_CdIdAz        2
010400170731     C                   parm      *blanks       pIn_MaskPDF      50
010500170731     C                   parm      *blanks       pOut_Esito        1
010600170731     C                   endif
010700000616     C*
010800010201     C                   seton                                        LR
010900990908
011000000801
011100910830     C*--------------------------------------------------------
011200140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
011300910830     C*--------------------------------------------------------
011400040526     C     RWFILE        BEGSR
011500990910     C*
011600990914     C                   if        not %open(tivin00r)
011700990908     C                   open      tivin00r
011800990914     C                   endif
011900140117     C                   if        not %open(edivabwr)
012000140117     C                   open      edivabwr
012100990914     C                   endif
012200141028     C*
012300140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
012400020305     C                   exsr      prevat
012500010201     C*
012600010202     C                   if        chkcall = '0'
012700010202     C*
012800140117     C                   if        not %open(edivatwr)
012900140117     C                   open      edivatwr
013000010201     C                   endif
013100990910     C*
013200010201     C                   clear                   §CTROKVB          5 0
013300020305     C                   clear                   §CTROKVT          5 0
013400000801     C                   clear                   §CTRMO            5 0
013500000801     C                   clear                   §CTRNO            5 0
013600141028     C*
013700141028     C* Inizializzazioni fuori ciclo
013800141028     C                   EXSR      INZVAR
013900140117     C*
014000140902     C                   if        *in53
014100140117     C                   call      'TIS7P2SER'
014200140117     C                   parm      'O'           pIn_Opz           1
014300140117     C                   parm                    tivlrds
014400140117     C                   parm                    EDIVABDS
014500140117     C                   parm      *blanks       pIn_CdIdAz        2
014600140117     C                   parm      *blanks       pIn_MaskPDF      50
014700140117     C                   parm      *blanks       pOut_Esito        1
014800140902     C                   endif
014900130708     C*
015000921023     C                   DO        *HIVAL
015100990913     C*
015200990915     C                   READ      tivin00r                               70
015300040910     C                   if        vindta > *blanks
015400000613     C                   add       1             rrnum
015500000801     C*
015600000801     C                   if        *in70 = *off
015700000801     C                             and
015800000801     C                             (vinflg = *blanks
015900000801     C                              or vinflg = '0'
016000000801     C                              or vinflg = '2')
016100000801     C*
016200000801     C                   clear                   vinmsg
016300141028     C*
016400141028     C* "normalizzo" il dato di input
016500141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
016600141028     C                                             %len(%trim(vindta))-1))
016700160209     C*
016800160209     C* Elimino le "duple di controllo EDIFACT"
016900160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017000160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
017100160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
017200160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
017300160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
017400040910     C*
017500040910     C* Eseguo routine d traduzione
017600040910     C                   exsr      impvabvat
017700040802     C*
017800010305     C                   endif
017900000905     C*
018000000905     C                   else
018100000905     C                   eval      vinflg = '1'
018200000905     C                   endif
018300000905     C*
018400000905     C  N70              update    tivin000
018500000905     C*
018600991022     C  N70              ENDdo
018700100722     C*
018800100722     C* Scarico i buffer testata ancora "in canna"
018900140922     C*
019000141028     C***  N31              ADD       1             §CTROKVB
019100141028     C***  N31              exsr      CHKWRI
019200141028     C***  N31              WRITE     EDIVAB00
019300010202     C*
019400010202     C                   endif
019500990910
019600990910     C* Se non ci sono record con errori ...
019700000710     C                   if        §ctrno = 0
019800990910     C* ... restituisco esito OK.
019900990921     C                   eval      wrkesito = '0'
020000990910     C                   else
020100010201     C                   if        §ctrokvb > 0
020200990921     C                   eval      wrkesito = '1'
020300000710     C                   else
020400000710     C                   eval      wrkesito = '2'
020500990910     C                   endif
020600000710     C                   endif
020700990910     C*
020800990914     C                   if        %open(tivin00r)
020900990908     C                   close     tivin00r
021000990914     C                   endif
021100140117     C                   if        %open(edivabwr)
021200140117     C                   close     edivabwr
021300990914     C                   endif
021400140117     C                   if        %open(edivatwr)
021500140117     C                   close     edivatwr
021600010201     C                   endif
021700990910     C*
021800010201     C                   if        §ctrokvb > 0
021900000724     C                             and vlrpoi <> *zeros
022000010202     C                   exsr      invio
022100990920     C                   endif
022200990920     C*
022300910830     C                   ENDSR
022400000613     C***
022500140922
022600140922     C*----------------------------------------------------*
022700140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
022800140922     C*----------------------------------------------------*
022900140922     C     CHKWRI        BEGSR
023000140922     C*
023100141028     C                   if        VABRMN = *zeros
023200141028     C                   eval      VABRMN = VABNSP
023300141028     C                   endif
023400151102     C*
023500151102     C                   if        VABRMA = *blanks
023600151102     C                   eval      VABRMA = savCMR
023700151102     C                   endif
023800160803     C*
023900160803     C                   eval      wNOTE  = %trim(wNOTE)
024000160803     C                   eval      VABNOT = %subst(wNOTE:1:35)
024100160803     C                   eval      VABNT2 = %subst(wNOTE:36:35)
024200141028     C*
024300141028     C                   if        VABPRD = *blanks
024400141028     C* ......VABPRD
024500141028     C* Reperisco la provincia dal CAP e dalla località
024600141028     C                   IF        VABLOD <> *blanks AND
024700141028     C                             VABCAD <> *blanks AND
024800141028     C                             VABNZD  = *blanks
024900141028     C                   CLEAR                   TISI95DS
025000141028     C                   EVAL      I95TCN = '3'
025100141028     C                   Z-ADD     datcor        I95DAT
025200141028     C                   EVAL      I95CAP = VABCAD
025300141028     C                   EVAL      I95LOC = VABLOD
025400141028     C                   CALL      'TISI95R'
025500141028     C                   PARM                    TISI95DS
025600141028     C                   EVAL      VABPRD = O95PRV
025700141028     C                   ENDIF
025800141028     C                   endif
025900140922     C*
026000140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
026100141028     C                   IF        wFlgCAS <> '0'
026200140922     C                   IF        VABCBO = '1'
026300140922     C                   EVAL      VABCBO = '4'
026400140922     C                   ELSE
026500140922     C                   EVAL      VABCBO = '6'
026600140922     C                   ENDIF
026700140922     C                   ENDIF
026800140922     C*
026900140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027000141028     C                   EXSR      CHKIMPDIV
027100140922     C*
027200140922     C                   ENDSR
027300141028     C*----------------------------------------------------*
027400141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500141028     C*----------------------------------------------------*
027600141028     C     INZVAR        BEGSR
027700141028     C*
027800141028     C* Inizializzo variabili di wrk
027900141028     C                   Z-ADD     *zeros        Num5_0            5 0
028000141028     C                   MOVEL     '0'           wFlgCAS
028100141028     C                   MOVEL     *blanks       wVATNOT_A
028200141028     C                   MOVEL     *blanks       wVATNOT_B
028300141028     C                   MOVEL     *blanks       wVATNOT_E
028400141028     C                   MOVEL     *blanks       wVATNOT_I
028500141028     C                   MOVEL     *blanks       wVATNOT_J
028600141028     C                   MOVEL     *blanks       wVATNOT_S
028700141028     C                   MOVEL     *blanks       wVATNOT_P
028800141028     C                   CLEAR                   SkSplitCSV_5
028900141028     C*
029000141028     C* Reimposto i valori di default
029100141028     C                   EXSR      DEFCAM
029200141028     C*
029300141028     C                   ENDSR
029400141028     C*----------------------------------------------------*
029500141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029600141028     C*----------------------------------------------------*
029700141028     C     DEFCAM        BEGSR
029800141028     C*
029900141028     C                   CLEAR                   EDIVAB00
030000141028     C                   CLEAR                   EDIVAT00
030100141028     C*
030200141028     C* Imposto i valori bolla di default
030300141028     C                   EVAL      VABCTR = 000
030400141028     C                   EVAL      VABCBO = '1'
030500141104     C                   EVAL      VABTSP = 'C'
030600141028     C*
030700141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030800141028     C* e delimitatore testo.
030900141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
031000141028     C*
031100141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
031200141028     C                   EVAL      CharSOS = CharNUM
031300141028     C*
031400141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
031500141028     C                   SETOFF                                       53
031600141028     C                   SELECT
031700141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
031800141028     C                   SETON                                        53
031900141028     C                   ENDSL
032000141028     C*
032100141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032200141028     C                   EVAL      posDaDft = 1
032300141028     C                   EVAL      posADft  = 0
032400141028     C                   EVAL      wGiroDft = 0
032500141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032600141028     C                             posDaDft > 0
032700141028     C*
032800141028     C* Gestisco il 1° giro
032900141028     C                   IF        wGiroDft = 0
033000141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
033100141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033200141028     C* Incremento il contatore dei "giri"
033300141028     C                   EVAL      wGiroDft = 1
033400141028     C                   ELSE
033500141028     C                   EVAL      posDaDft = posADft
033600141028     C                   ENDIF
033700141028     C* Eseguo lo scan x trovare la fine del campo corrente
033800141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033900141028     C*
034000141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
034100141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
034200141028     C                   IF        posDaDft > 0 AND
034300141028     C                             posADft  > 0
034400141028     C* NCL
034500141028     C                   IF        %subst(
034600141028     C                             %subst(vlrppt:posDaDft+1:
034700141028     C                             posADft-posDaDft-1):1:3)
034800141028     C                             = 'NCL'
034900141028     C                   EVAL      PiStr=%trim(%subst(
035000141028     C                             %subst(vlrppt:posDaDft+1:
035100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035200141028     C                   EXSR      CHKNUM
035300141028     C                   IF        PiInt=*on
035400141028     C                   Z-ADD     PiVal         VABNCL
035500141028     C                   ENDIF
035600141028     C                   ENDIF
035700141028     C* CCM
035800141028     C                   IF        %subst(
035900141028     C                             %subst(vlrppt:posDaDft+1:
036000141028     C                             posADft-posDaDft-1):1:3)
036100141028     C                             = 'CCM'
036200141028     C                   EVAL      PiStr=%trim(%subst(
036300141028     C                             %subst(vlrppt:posDaDft+1:
036400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500141028     C                   EXSR      CHKNUM
036600141028     C                   IF        PiInt=*on
036700141028     C                   Z-ADD     PiVal         VABCCM
036800141028     C                   ENDIF
036900141028     C                   ENDIF
037000141028     C* LNP
037100141028     C                   IF        %subst(
037200141028     C                             %subst(vlrppt:posDaDft+1:
037300141028     C                             posADft-posDaDft-1):1:3)
037400141028     C                             = 'LNP'
037500141028     C                   EVAL      PiStr=%trim(%subst(
037600141028     C                             %subst(vlrppt:posDaDft+1:
037700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800141028     C                   EXSR      CHKNUM
037900141028     C                   IF        PiInt=*on
038000141028     C                   Z-ADD     PiVal         VABLNP
038100141028     C                   ENDIF
038200141028     C                   ENDIF
038300141028     C* NRS
038400141028     C                   IF        %subst(
038500141028     C                             %subst(vlrppt:posDaDft+1:
038600141028     C                             posADft-posDaDft-1):1:3)
038700141028     C                             = 'NRS'
038800141028     C                   EVAL      PiStr=%trim(%subst(
038900141028     C                             %subst(vlrppt:posDaDft+1:
039000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039100141028     C                   EXSR      CHKNUM
039200141028     C                   IF        PiInt=*on
039300141028     C                   Z-ADD     PiVal         VABNRS
039400141028     C                   ENDIF
039500141028     C                   ENDIF
039600141028     C* CTR
039700141028     C                   IF        %subst(
039800141028     C                             %subst(vlrppt:posDaDft+1:
039900141028     C                             posADft-posDaDft-1):1:3)
040000141028     C                             = 'CTR'
040100141028     C                   EVAL      PiStr=%trim(%subst(
040200141028     C                             %subst(vlrppt:posDaDft+1:
040300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040400141028     C                   EXSR      CHKNUM
040500141028     C                   IF        PiInt=*on
040600141028     C                   Z-ADD     PiVal         VABCTR
040700141028     C                   ENDIF
040800141028     C                   ENDIF
040900141028     C* PKB
041000141028     C                   IF        %subst(
041100141028     C                             %subst(vlrppt:posDaDft+1:
041200141028     C                             posADft-posDaDft-1):1:3)
041300141028     C                             = 'PKB'
041400141028     C                   EVAL      PiStr=%trim(%subst(
041500141028     C                             %subst(vlrppt:posDaDft+1:
041600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041700141028     C                   EXSR      CHKNUM
041800141028     C                   IF        PiNum=*on
041900141028     C                   Z-ADD     PiVal         VABPKB
042000141028     C                   ENDIF
042100141028     C                   ENDIF
042200141028     C* VLB
042300141028     C                   IF        %subst(
042400141028     C                             %subst(vlrppt:posDaDft+1:
042500141028     C                             posADft-posDaDft-1):1:3)
042600141028     C                             = 'VLB'
042700141028     C                   EVAL      PiStr=%trim(%subst(
042800141028     C                             %subst(vlrppt:posDaDft+1:
042900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043000141028     C                   EXSR      CHKNUM
043100141028     C                   IF        PiNum=*on
043200141028     C                   Z-ADD     PiVal         VABVLB
043300141028     C                   ENDIF
043400141028     C                   ENDIF
043500141028     C* QFT
043600141028     C                   IF        %subst(
043700141028     C                             %subst(vlrppt:posDaDft+1:
043800141028     C                             posADft-posDaDft-1):1:3)
043900141028     C                             = 'QFT'
044000141028     C                   EVAL      PiStr=%trim(%subst(
044100141028     C                             %subst(vlrppt:posDaDft+1:
044200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044300141028     C                   EXSR      CHKNUM
044400141028     C                   IF        PiNum=*on
044500141028     C                   Z-ADD     PiVal         VABQFT
044600141028     C                   ENDIF
044700141028     C                   ENDIF
044800141028     C* CBO
044900141028     C                   IF        %subst(
045000141028     C                             %subst(vlrppt:posDaDft+1:
045100141028     C                             posADft-posDaDft-1):1:3)
045200141028     C                             = 'CBO'
045300141028     C                   EVAL      VABCBO=%trim(%subst(
045400141028     C                             %subst(vlrppt:posDaDft+1:
045500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045600141028     C                   ENDIF
045700141028     C* TSP
045800141028     C                   IF        %subst(
045900141028     C                             %subst(vlrppt:posDaDft+1:
046000141028     C                             posADft-posDaDft-1):1:3)
046100141028     C                             = 'TSP'
046200141028     C                   EVAL      VABTSP=%trim(%subst(
046300141028     C                             %subst(vlrppt:posDaDft+1:
046400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046500141028     C                   ENDIF
046600141028     C* VAS
046700141028     C                   IF        %subst(
046800141028     C                             %subst(vlrppt:posDaDft+1:
046900141028     C                             posADft-posDaDft-1):1:3)
047000141028     C                             = 'VAS'
047100141028     C                   EVAL      VABVAS=%trim(%subst(
047200141028     C                             %subst(vlrppt:posDaDft+1:
047300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047400141028     C                   ENDIF
047500141028     C* VCA
047600141028     C                   IF        %subst(
047700141028     C                             %subst(vlrppt:posDaDft+1:
047800141028     C                             posADft-posDaDft-1):1:3)
047900141028     C                             = 'VCA'
048000141028     C                   EVAL      VABVCA=%trim(%subst(
048100141028     C                             %subst(vlrppt:posDaDft+1:
048200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048300141028     C                   ENDIF
048400141028     C* TIC
048500141028     C                   IF        %subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):1:3)
048800141028     C                             = 'TIC'
048900141028     C                   EVAL      VABTIC=%trim(%subst(
049000141028     C                             %subst(vlrppt:posDaDft+1:
049100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049200141028     C                   ENDIF
049300141028     C* GCA
049400141028     C                   IF        %subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):1:3)
049700141028     C                             = 'GCA'
049800141028     C                   EVAL      VABGCA=%trim(%subst(
049900141028     C                             %subst(vlrppt:posDaDft+1:
050000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100141028     C                   ENDIF
050200141028     C* CTM
050300141028     C                   IF        %subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):1:3)
050600141028     C                             = 'CTM'
050700141028     C                   EVAL      VABCTM=%trim(%subst(
050800141028     C                             %subst(vlrppt:posDaDft+1:
050900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000141028     C                   ENDIF
051100141028     C* FFD
051200141028     C                   IF        %subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):1:3)
051500141028     C                             = 'FFD'
051600141028     C                   EVAL      VABFFD=%trim(%subst(
051700141028     C                             %subst(vlrppt:posDaDft+1:
051800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051900141028     C                   ENDIF
052000141028     C* VAD
052100141028     C                   IF        %subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):1:3)
052400141028     C                             = 'VAD'
052500141028     C                   EVAL      VABVAD=%trim(%subst(
052600141028     C                             %subst(vlrppt:posDaDft+1:
052700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052800141028     C                   ENDIF
052900141028     C* GMA
053000141028     C                   IF        %subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):1:3)
053300141028     C                             = 'GMA'
053400141028     C                   EVAL      VABGMA=%trim(%subst(
053500141028     C                             %subst(vlrppt:posDaDft+1:
053600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700141028     C                   ENDIF
053800141028     C* GGA
053900141028     C                   IF        %subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):1:3)
054200141028     C                             = 'GGA'
054300141028     C                   EVAL      VABGGA=%trim(%subst(
054400141028     C                             %subst(vlrppt:posDaDft+1:
054500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600141028     C                   ENDIF
054700141028     C* GVA
054800141028     C                   IF        %subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000141028     C                             posADft-posDaDft-1):1:3)
055100141028     C                             = 'GVA'
055200141028     C                   EVAL      VABGVA=%trim(%subst(
055300141028     C                             %subst(vlrppt:posDaDft+1:
055400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500141028     C                   ENDIF
055600141028     C* TC1
055700141028     C                   IF        %subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):1:3)
056000141028     C                             = 'TC1'
056100141028     C                   EVAL      VABTC1=%trim(%subst(
056200141028     C                             %subst(vlrppt:posDaDft+1:
056300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400141028     C                   ENDIF
056500141028     C* TC2
056600141028     C                   IF        %subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):1:3)
056900141028     C                             = 'TC2'
057000141028     C                   EVAL      VABTC2=%trim(%subst(
057100141028     C                             %subst(vlrppt:posDaDft+1:
057200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300141028     C                   ENDIF
057400141028     C* VATTRC
057500141028     C                   IF        %subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):1:3)
057800141028     C                             = 'TRC'
057900141028     C                   EVAL      VATTRC=%trim(%subst(
058000141028     C                             %subst(vlrppt:posDaDft+1:
058100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200141028     C                   ENDIF
058300141028     C* ...
058400141028     C                   ENDIF
058500141028     C                   ENDDO
058600141028     C*
058700141028     C                   ENDSR
058800000801     C*----------------------------------------------------*
058900140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
059000000801     C*----------------------------------------------------*
059100040910     C     IMPVABVAT     BEGSR
059200040910     C*
059300040910     C* Traduzione relativa ai tipi record del file del cliente
059400040910     C*
059500071210     C*
059600071210     C***
059700141028     C* ...tipo record 'UNB' (info CMR)
059800141028     C                   EVAL      wTag = 'UNB'
059900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
060000141028     C                   MOVEL     *blanks       savCMR           35
060100141028     C                   EVAL      InStringa =
060200141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
060300141028     C                   EVAL      InSepara = '+'
060400141028     C                   EXSR      SR_SPLIT
060500141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
060600140922     C                                      %char(datcor) + ' ' + %char(oracor)
060700130620     C                   ENDIF
060800141028     C*
060900141028     C***
061000141028     C* ...tipo record 'UNH' (rottuta di codice spedizione)
061100141028     C                   EVAL      wTag = 'UNH'
061200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061300141028     C                   SETOFF                                       31
061400141028     C                   eval      vinflg = '1'
061500141028     C* ......inizializzazioni iniziali e formati record file Bartolini
061600141028     C                   EXSR      INZVAR
061700160803     C                   MOVEL     *blanks       wNOTE            70
061800141028     C* ......valorizzazione campi fissi
061900141028     C                   MOVEL     datcor        VABAAS
062000141028     C                   MOVE      datcor        VABMGS
062100141028     C                   MOVE(P)   vlrpoi        VABFGS
062200141028     C                   EVAL      VABCMR = savCMR
062300141028     C                   EVAL      VABDCM = datcor
062400141028     C                   EVAL      VABDTS = datcor
062500141028     C                   EVAL      VABCNT = 1
062600141028     C* ......NSP => Stacco un numeratore da AZNUM
062700141028     C                   clear                   TRUL33DS
062800141028     C                   eval      I33OPE = *zeros
062900141028     C                   eval      I33CNU = 302
063000141028     C                   eval      I33NUM = 1
063100141028     C                   movel     TRUL33DS      KPJBU
063200141028     C                   call      'TRUL33R'
063300141028     C                   parm                    KPJBA
063400141028     C                   movel     KPJBU         TRUL33DS
063500141028     C                   if        O33ERR = *zeros
063600141028     C                   z-add     O33NRF        VABNSP
063700141028     C                   else
063800141028     C                   Z-ADD     1             wRecErr
063900141028     C                   SETON                                        31
064000141028     C                   EVAL      vinmsg = %trimr(vinmsg)
064100141028     C                             + ' ' + 'VABNSP VATNSP'
064200141028     C                   endif
064300141028     C                   ENDIF
064400160803     C*
064500160803     C***
064600160803     C* ...tipo record 'DTM+137' (data consegna richiesta)
064700160803     C                   EVAL      wTag = 'DTM+137'
064800160803     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064900160803     C                   EVAL      PiStr=%subst(
065000160803     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
065100160803     C                   EXSR      CHKNUM
065200160803     C                   IF        PiInt=*on
065300160803     C                   Z-ADD     PiVal         VABDCR
065400160803     C                   ELSE
065500160803     C                   Z-ADD     1             wRecErr
065600160803     C                   ENDIF
065700160803     C                   ENDIF
065800160803     C*
065900160803     C***
066000160803     C* ...tipo record 'TSR' (special services)
066100160803     C                   EVAL      wTag = 'TSR'
066200160803     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066300160803     C                   EVAL      InStringa =
066400160803     C                                %subst(wvindta:%len(%trim(wTag))+2)
066500160803     C                   EVAL      InSepara = '+'
066600160803     C                   EXSR      SR_SPLIT
066700160803     C                   IF        %trim(SkSplitCSV_5(1)) = 'EXP'
066800160803     C                   EVAL      VABTSP = 'H'
066900160803     C                   ENDIF
067000160803     C                   ENDIF
067100160803     C*
067200160803     C***
067300160803     C* ...tipo record 'FTX+TRA' (note di consegna)
067400160803     C                   EVAL      wTag = 'FTX+TRA'
067500160803     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067600160803     C                   EVAL      InStringa =
067700160803     C                                %subst(wvindta:%len(%trim(wTag))+2)
067800160803     C                   EVAL      InSepara = '+'
067900160803     C                   EXSR      SR_SPLIT
068000160803     C                   IF        SkSplitCSV_5(1)='PA'
068100160803     C                   EVAL      VABTC1 = 'A'
068200160803     C                   ENDIF
068300160803     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
068400160803     C                                      %trim(SkSplitCSV_5(3))
068500160803     C                   ENDIF
068600160803     C*
068700160803     C***
068800160805     C* ...tipo record 'BGM+340' (riferimento spedizione)
068900160805     C                   EVAL      wTag = 'BGM+340'
069000160803     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069100170821     C                   EVAL      VABRMA = %subst(wvindta:%len(%trim(wTag))+2)
069200160803     C                   ENDIF
069300151124     C*
069400151124     C***
069500160803     C* ...tipo record 'RFF+OR' (riferimenti spedizione)
069600170821     C                   EVAL      wTag = 'RFF+OR'
069700170821     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069800170821     C                   EVAL      InStringa =
069900170821     C                                %subst(wvindta:%len(%trim(wTag))+2)
070000170821     C                   EVAL      InSepara = '+'
070100170821     C                   EXSR      SR_SPLIT
070200151124     C* RMN
070300170821     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
070400170821     C                   EXSR      CHKNUM
070500170821     C                   IF        PiInt=*on
070600170821     C                   Z-ADD     PiVal         VABRMN
070700170821     C                   ELSE
070800170821     C                   Z-ADD     1             wRecErr
070900170821     C                   ENDIF
071000170821     C                   ENDIF
071100141028     C*
071200141028     C***
071300141028     C* ...tipo record 'NAD+CN' (destinatario)
071400141028     C                   EVAL      wTag = 'NAD+CN'
071500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071600141028     C                   EVAL      InStringa =
071700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
071800141028     C                   EVAL      InSepara = '+'
071900141028     C                   EXSR      SR_SPLIT
072000141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
072100141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
072200141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
072300141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
072400141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
072500141028     C                   IF        VABNZD = 'I'   OR
072600141028     C                             VABNZD = 'IT'  OR
072700141028     C                             VABNZD = 'ITA'
072800141028     C                   EVAL      VABNZD = *blanks
072900141028     C                   ENDIF
073000141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
073100141028     C                   EVAL      InSepara = ':'
073200141028     C                   EXSR      SR_SPLIT
073300141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
073400141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
073500160803     C***                EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
073600160803     C***                                   %trim(SkSplitCSV_5(4))+
073700160803     C***                                   %trim(SkSplitCSV_5(5))
073800141028     C                   ENDIF
073900141104     C*
074000141104     C***
074100160803     C* ...tipo record 'NAD+CZ' (mittente originale)
074200160803     C                   EVAL      wTag = 'NAD+CZ'
074300141104     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074400141104     C                   EVAL      InStringa =
074500141104     C                                %subst(wvindta:%len(%trim(wTag))+2)
074600141104     C                   EVAL      InSepara = '+'
074700141104     C                   EXSR      SR_SPLIT
074800160803     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
074900160803     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
075000160803     C                   IF        VABNMO = 'I'   OR
075100160803     C                             VABNMO = 'IT'  OR
075200160803     C                             VABNMO = 'ITA'
075300160803     C                   EVAL      VABNMO = *blanks
075400141104     C                   ENDIF
075500141104     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
075600141104     C                   EVAL      InSepara = ':'
075700141104     C                   EXSR      SR_SPLIT
075800160803     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
075900141104     C                   ENDIF
076000141028     C*
076100141028     C***
076200141028     C* ...tipo record 'GID' (colli)
076300141028     C                   EVAL      wTag = 'GID'
076400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076500141028     C                   EVAL      InStringa =
076600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
076700141028     C                   EVAL      InSepara = '+'
076800141028     C                   EXSR      SR_SPLIT
076900160201     C                   EVAL      InStringa = %trim(SkSplitCSV_5(2))
077000160201     C                   EVAL      InSepara = ':'
077100160201     C                   EXSR      SR_SPLIT
077200160201     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
077300141028     C                   EXSR      CHKNUM
077400141028     C                   IF        PiInt=*on
077500170830     C                   ADD       PiVal         VABNCL
077600141028     C                   ELSE
077700141028     C                   Z-ADD     1             wRecErr
077800141028     C                   ENDIF
077900141028     C                   ENDIF
078000141028     C*
078100141028     C***
078200141028     C* ...tipo record 'FTX+AAA' (natura merce)
078300141028     C                   EVAL      wTag = 'FTX+AAA'
078400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078500141028     C                   EVAL      InStringa =
078600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
078700141028     C                   EVAL      InSepara = '+'
078800141028     C                   EXSR      SR_SPLIT
078900141028     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
079000141028     C                   ENDIF
079100141028     C*
079200141028     C***
079300160803     C* ...tipo record 'MEA+WT' (peso spedizione)
079400160803     C                   EVAL      wTag = 'MEA+WT'
079500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079600141028     C                   EVAL      InStringa =
079700141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
079800141028     C                   EVAL      InSepara = ':'
079900141028     C                   EXSR      SR_SPLIT
080000141028     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
080100141028     C                   EXSR      CHKNUM
080200141028     C                   IF        PiNum=*on
080300150924     C                   ADD       PiVal         VABPKB
080400141028     C                   ELSE
080500141028     C                   Z-ADD     1             wRecErr
080600141028     C                   ENDIF
080700141028     C                   ENDIF
080800141028     C*
080900141028     C***
081000141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
081100160902     C***                EVAL      wTag = 'MEA+VOL'
081200160902     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081300160902     C***                EVAL      InStringa =
081400160902     C***                             %subst(wvindta:%len(%trim(wTag))+2)
081500160902     C***                EVAL      InSepara = ':'
081600160902     C***                EXSR      SR_SPLIT
081700160902     C***                EVAL      PiStr=%trim(SkSplitCSV_5(2))
081800160902     C***                EXSR      CHKNUM
081900160902     C***                IF        PiNum=*on
082000160902     C***                ADD       PiVal         VABVLB
082100160902     C***                ELSE
082200160902     C***                Z-ADD     1             wRecErr
082300160902     C***                ENDIF
082400160902     C***                ENDIF
082500141028     C*
082600141028     C***
082700160803     C* ...tipo record 'PCI+18' (dettagli segnacolli)
082800160803     C                   EVAL      wTag = 'PCI+18'
082900151218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083000151218     C                   EVAL      InStringa =
083100151218     C                                %subst(wvindta:%len(%trim(wTag))+2)
083200151218     C                   EVAL      InSepara = '+'
083300151218     C                   EXSR      SR_SPLIT
083400151218     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
083500151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
083600151218     C                   EVAL      VATTRC = 'E'
083700151218     C                   EXSR      WRIVAT
083800151218     C                   ENDIF
083900151218     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
084000151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
084100151218     C                   EVAL      VATTRC = 'E'
084200151218     C                   EXSR      WRIVAT
084300151218     C                   ENDIF
084400151218     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
084500151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
084600151218     C                   EVAL      VATTRC = 'E'
084700151218     C                   EXSR      WRIVAT
084800151218     C                   ENDIF
084900151218     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
085000151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
085100151218     C                   EVAL      VATTRC = 'E'
085200151218     C                   EXSR      WRIVAT
085300151218     C                   ENDIF
085400151218     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
085500151218     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
085600151218     C                   EVAL      VATTRC = 'E'
085700151218     C                   EXSR      WRIVAT
085800151218     C                   ENDIF
085900151218     C                   ENDIF
086000141028     C*
086100141028     C***
086200141028     C* ...tipo record '???' (PDF per packing-list)
086300141028     C                   EVAL      wTag = '???PDF'
086400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086500141028     C*
086600141028     C* ......VATNOT_P
086700141028     C                   if        *in53
086800141028     C*
086900141028     C* Solo se valorizzato RMN
087000141028     C                   if        VABRMA <> *blanks
087100141028     C                   eval      pIn_MaskPDF =
087200141028     C                                     %trim(%subst(wvindta:1292:100))
087300141028     C                   call      'TIS7P2SER'
087400141028     C                   parm      'W'           pIn_Opz           1
087500141028     C                   parm                    tivlrds
087600141028     C                   parm                    EDIVABDS
087700141028     C                   parm      '10'          pIn_CdIdAz        2
087800141028     C                   parm                    pIn_MaskPDF      50
087900141028     C                   parm      *blanks       pOut_Esito        1
088000141028     C                   endif
088100141028     C                   endif
088200141028     C*
088300141028     C                   ENDIF
088400141028     C*
088500141028     C***
088600141028     C* ...tipo record 'UNT' (fine bolla)
088700141028     C                   EVAL      wTag = 'UNT'
088800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088900141028     C  N31              ADD       1             §CTROKVB
089000141028     C  N31              exsr      CHKWRI
089100140117     C  N31              WRITE     EDIVAB00
089200100722     C                   endif
089300010202     C*
089400000801     C* Ebbene...
089500000801     C                   ADD       1             §CTRMO
089600141028     C                   IF        wRecErr <> *zeros
089700000801     C                   ADD       1             §CTRNO
089800000801     C                   EVAL      vinflg = '2'
089900000801     C                   ENDIF
090000000801     C*
090100000801     C                   ENDSR
090200010202     C*----------------------------------------------------*
090300140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
090400010202     C*----------------------------------------------------*
090500020305     C     PREVAT        BEGSR
090600010202     C*
090700140117     C* Compongo il nome del membro da dare al EDIVATWR
090800010202     C                   eval      parmbr = vlrhdl
090900010202     C                   movel     'M'           parmbr
091000060113     C                   eval      parccm = vlrksc
091100010202     C                   eval      paropz = '1'
091200010202     C* Effettuo la chiamata al CLLE preposto
091300140117     C                   call(e)   'TITVEVTC'
091400010202     C                   parm                    parccm
091500010202     C                   parm                    parmbr
091600010202     C                   parm                    paropz
091700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
091800010202     C                   if        %error
091900010202     C                   movel     '1'           chkcall
092000010202     C                   else
092100010202     C                   movel     '0'           chkcall
092200010202     C                   endif
092300010202     C*
092400010202     C                   ENDSR
092500141028     C*----------------------------------------------------*
092600141028     C*  CONTROLLO NUMERICITA' CAMPI
092700141028     C*----------------------------------------------------*
092800141028     C     CHKNUM        BEGSR
092900141028     C*
093000141028     C                   IF        PiDecChr = *blanks
093100141028     C                   EVAL      PiDecChr = CharNUM
093200141028     C                   ENDIF
093300141028     C*
093400141028     C                   callp     UBISNUM_Check(PiStr
093500141028     C                                          :PiDecChr
093600141028     C                                          :PiVal
093700141028     C                                          :PiNum
093800141028     C                                          :PiInt)
093900141028     C*
094000141028     C                   ENDSR
094100141028     C***
094200141028
094300141028
094400141028     C*----------------------------------------------------*
094500141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
094600141028     C*----------------------------------------------------*
094700141028     C     SR_SPLIT      BEGSR
094800141028     C*
094900141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
095000141028     C                   CALL      'XSPLIT2'
095100141028     C                   PARM                    InStringa
095200141028     C                   PARM                    InSepara
095300141028     C                   PARM      '5'           InTypeOut
095400141028     C                   PARM      ''            wSkParam
095500141028     C                   PARM                    OutErrore
095600141028     C                   PARM                    OutMsg
095700141028     C                   MOVEA     wSkParam      SkSplitCSV_5
095800141028     C*
095900141028     C                   ENDSR
096000141028
096100141028
096200141028     C*----------------------------------------------------*
096300141028     C*  SCRITTURA BUFFER EDIVAT
096400141028     C*----------------------------------------------------*
096500141028     C     WRIVAT        BEGSR
096600141028     C*
096700141028     C                   IF        VATNOT <> *blanks
096800141028     C                   EVAL      VATFGS = VABFGS
096900141028     C                   EVAL      VATCCM = VABCCM
097000141028     C                   EVAL      VATLNP = VABLNP
097100141028     C                   EVAL      VATAAS = VABAAS
097200141028     C                   EVAL      VATNRS = VABNRS
097300141028     C                   EVAL      VATNSP = VABNSP
097400141028     C                   EVAL      VATCMR = VABCMR
097500141028     C                   EVAL      VATCNT = VABCNT
097600141028     C                   ADD       1             §CTROKVT
097700141028     C                   WRITE     EDIVAT00
097800141028     C                   ENDIF
097900141028     C*
098000141028     C                   ENDSR
098100000801
098200011113
098300011113     C*----------------------------------------------------*
098400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
098500011113     C*----------------------------------------------------*
098600011113     C     CHKIMPDIV     BEGSR
098700011113     C*
098800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
098900011113     C                   Z-ADD     *zeros        wrkDec            9 9
099000011113     C*
099100011113     C* Come prima cosa effettuo considerazioni sulla divisa
099200011113     C                   IF        vabIAS > *zeros
099300011113     C                   IF        vabVAS <> 'EUR'
099400011113     C                   EVAL      vabVAS =  'ITL'
099500011113     C                   ENDIF
099600011113     C                   ENDIF
099700011113     C*
099800011113     C                   IF        vabCAS > *zeros
099900011113     C                   IF        vabVCA <> 'EUR'
100000011113     C                   EVAL      vabVCA =  'ITL'
100100011113     C                   ENDIF
100200011113     C                   ENDIF
100300011113     C*
100400011113     C                   IF        vabVMD > *zeros
100500020305     C                   IF        vabVAD <> 'EUR'
100600011113     C                   EVAL      vabVAD =  'ITL'
100700011113     C                   ENDIF
100800011113     C                   ENDIF
100900011113     C*
101000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
101100011113     C                   Z-ADD     vabIAS        wrkDec
101200011113     C                   IF        wrkDec > *zeros
101300011113     C                   IF        vabVAS = 'ITL'
101400011113     C                   EVAL      vabIAS = *zeros
101500011113     C                   ENDIF
101600011113     C                   ENDIF
101700011113     C*
101800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
101900011113     C                   Z-ADD     vabCAS        wrkDec
102000011113     C                   IF        wrkDec > *zeros
102100011113     C                   IF        vabVCA = 'ITL'
102200011113     C                   EVAL      vabCAS = *zeros
102300011113     C                   ENDIF
102400011113     C                   ENDIF
102500011113     C*
102600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
102700011113     C                   Z-ADD     vabVMD        wrkDec
102800011113     C                   IF        wrkDec > *zeros
102900011113     C                   IF        vabVAD = 'ITL'
103000011113     C                   EVAL      vabVMD = *zeros
103100011113     C                   ENDIF
103200011113     C                   ENDIF
103300011113     C*
103400011113     C                   ENDSR
103500011113     C***
103600011113
103700011113
103800000801
103900000801
104000990920      /TITLE Invio dei dati al punto operativo.
104100010202     C     invio         BEGSR
104200990920     C*
104300140117     C* 1° invio EDIVAT
104400010201     C                   reset                   dscmz
104500010201     C                   move      vlrpoi        cmzdst
104600140117     C                   eval      cmzfld = 'EDIVATWR'
104700010201     C                   eval      cmzmbd = vlrhdl
104800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
104900021009     C***                if        prmfir = *blanks
105000140117     C                   eval      cmzfla = 'EDIVAT0F'
105100140117     C                   eval      cmzmba = 'EDIVAT0F'
105200021009     C***                else
105300021009     C***                eval      cmzfla = prmfir
105400021009     C***                eval      cmzmba = prmfir
105500021009     C***                endif
105600010201     C                   eval      cmznrr = *zeros
105700020305     C                   move      §ctrokvt      cmznrr
105800021018     C                   eval      cmzlba = vlrfl1
105900010201     C                   call(e)   'TIS711C'
106000010201     C                   parm                    dscmz
106100010201     C                   parm      *blanks       esito
106200010205     C                   if        %error
106300010205     C                             or cmzerr = '1'
106400010205     C                             or esito  = '1'
106500010205     C                   eval      wrkesito = '3'
106600010205     C                   else
106700010201     C*
106800140117     C* 2° invio EDIVAB
106900010201     C                   reset                   dscmz
107000010201     C                   move      vlrpoi        cmzdst
107100010201     C                   eval      cmzfld = vlrfou
107200010201     C                   eval      cmzmbd = vlrhdl
107300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
107400021009     C***                if        prmfir = *blanks
107500140117     C                   eval      cmzfla = 'EDIVAB0F'
107600140117     C                   eval      cmzmba = 'EDIVAB0F'
107700021009     C***                else
107800021009     C***                eval      cmzfla = prmfir
107900021009     C***                eval      cmzmba = prmfir
108000021009     C***                endif
108100010201     C                   eval      cmznrr = *zeros
108200010201     C                   move      §ctrokvb      cmznrr
108300021018     C                   eval      cmzlba = vlrfl1
108400010201     C                   call(e)   'TIS711C'
108500010201     C                   parm                    dscmz
108600010201     C                   parm      *blanks       esito
108700010201     C                   if        %error
108800010201     C                             or cmzerr = '1'
108900010201     C                             or esito  = '1'
109000010201     C                   eval      wrkesito = '3'
109100010201     C                   endif
109200010205     C                   endif
109300990920     C*
109400000613     C                   ENDSR
109500000613     C***
109600070411
109700141028
109800070411     C     *pssr         BEGSR
109900070411     C*
110000070411     C                   if        %open(tivin00r)
110100070411     C                   close     tivin00r
110200070411     C                   endif
110300140117     C                   if        %open(edivabwr)
110400140117     C                   close     edivabwr
110500070411     C                   endif
110600140117     C                   if        %open(edivatwr)
110700140117     C                   close     edivatwr
110800070411     C                   endif
110900070411     C*
111000070411     C* Effettuo la chiamata al CLLE preposto
111100140117     C                   call(e)   'TITVEVTC'
111200070411     C                   parm                    parccm
111300070411     C                   parm                    parmbr
111400070411     C                   parm      '2'           paropz
111500070411     C*
111600070411     C                   eval      wrkesito = '2'
111700070411     C*
111800070411     C                   seton                                        LR
111900070411     C*
112000070411     C                   ENDSR     '*CANCL'
112100070411     C***
112200070411
112300990910
112400000613     C     *inzsr        BEGSR
112500990910     C*
112600990910     C     *entry        plist
112700990920     C                   parm                    tivlrds
112800990921     C                   parm      wrkesito      esito
112900000724     C                   parm                    prmlit
113000000710     C                   parm                    prmfir
113100000613     C*
113200000830     C* CALCOLA LA DATA CORRENTE
113300140117     C                   time                    wn14             14 0
113400140117     C                   movel     wn14          oracor            6 0          *ORA
113500100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
113600100722     C                   eval      datcor = %dec(%date() : *ISO)
113700000830     C*
113800000613     C                   ENDSR
113900000613     C***
