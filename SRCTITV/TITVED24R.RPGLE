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
003000150527     D wNOTE           s             70    INZ(*blanks)
003100161206     D wNOTE_2         s             70    INZ(*blanks)
003200141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003300141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003800141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
003900141028
004000141028     D*------------------
004100141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004200141028     D*------------------
004300141028     D CharNUM         S              1
004400141028     D CharSOS         S              1
004500141028     D posDaDft        S              3  0 INZ(*zeros)
004600141028     D posADft         S              3  0 INZ(*zeros)
004700141028     D wGiroDft        s              1  0 INZ(*zeros)
004800141028
004900141028
005000141028     D*------------------
005100141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005200141028     D*------------------
005300141028     D InStringa       S          65535A   VARYING                              (stringa input)
005400141028     D InSepara        S             10A                                        (separatore)
005500141028     D InTypeOut       S              1A                                        (tipo output)
005600141028     D wSkParam        S          65535A                                        (output)
005700141028     D OutErrore       S              1A                                        (flag errore)
005800141028     D OutMsg          S             80A                                        (messaggio errore)
005900141028     D SkSplitCSV_5    S            256    DIM(256)
006000141028
006100000830
006200041025     D*------------------
006300041025     D* DS REPERIMENTO NUMERATORE
006400041025     D*------------------
006500041025     D trul33ds      e ds                  inz
006600041025     D*------------------
006700041025     D* DS ARCHITETTURA
006800041025     D*------------------
006900041025     D kpjba         e ds                  inz
007000041025     D*------------------
007100990908
007200141028
007300141028     D*------------------
007400141028     D* LINKING A DEFINIZIONI ESTERNE
007500141028     D*------------------
007600141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800150609
007900141028
008000150609     D*-------------------
008100150609     D* COSTANTI
008200150609     D*-------------------
008300150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500141028
008600010201
008700010201
008800000913     C                   reset                   rrnum
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100150526     C                   setoff                                       90
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400070719     C*
009500070719     C* Esegue lancio TISI95R solo x chiusura
009600070719     C                   CLEAR                   TISI95DS
009700070719     C                   EVAL      I95TLA = 'C'
009800070719     C                   CALL      'TISI95R'
009900070719     C                   PARM                    TISI95DS
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200140922     C                   call(e)   'TITVEVTC'
010300140922     C                   parm                    parccm
010400140922     C                   parm                    parmbr
010500140922     C                   parm      '2'           paropz
010600170731     C*
010700170731     C                   if        *in53
010800170731     C                   call      'TIS7P2SER'
010900170731     C                   parm      'C'           pIn_Opz           1
011000170731     C                   parm                    tivlrds
011100170731     C                   parm                    EDIVABDS
011200170731     C                   parm      *blanks       pIn_CdIdAz        2
011300170731     C                   parm      *blanks       pIn_MaskPDF      50
011400170731     C                   parm      *blanks       pOut_Esito        1
011500170731     C                   endif
011600000616     C*
011700010201     C                   seton                                        LR
011800990908
011900000801
012000910830     C*--------------------------------------------------------
012100140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012200910830     C*--------------------------------------------------------
012300040526     C     RWFILE        BEGSR
012400990910     C*
012500990914     C                   if        not %open(tivin00r)
012600990908     C                   open      tivin00r
012700990914     C                   endif
012800140117     C                   if        not %open(edivabwr)
012900140117     C                   open      edivabwr
013000990914     C                   endif
013100141028     C*
013200140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
013300020305     C                   exsr      prevat
013400010201     C*
013500010202     C                   if        chkcall = '0'
013600010202     C*
013700140117     C                   if        not %open(edivatwr)
013800140117     C                   open      edivatwr
013900010201     C                   endif
014000990910     C*
014100010201     C                   clear                   §CTROKVB          5 0
014200020305     C                   clear                   §CTROKVT          5 0
014300000801     C                   clear                   §CTRMO            5 0
014400000801     C                   clear                   §CTRNO            5 0
014500141028     C*
014600141028     C* Inizializzazioni fuori ciclo
014700141028     C                   EXSR      INZVAR
014800140117     C*
014900140902     C                   if        *in53
015000140117     C                   call      'TIS7P2SER'
015100140117     C                   parm      'O'           pIn_Opz           1
015200140117     C                   parm                    tivlrds
015300140117     C                   parm                    EDIVABDS
015400140117     C                   parm      *blanks       pIn_CdIdAz        2
015500140117     C                   parm      *blanks       pIn_MaskPDF      50
015600140117     C                   parm      *blanks       pOut_Esito        1
015700140902     C                   endif
015800130708     C*
015900921023     C                   DO        *HIVAL
016000990913     C*
016100990915     C                   READ      tivin00r                               70
016200040910     C                   if        vindta > *blanks
016300000613     C                   add       1             rrnum
016400000801     C*
016500000801     C                   if        *in70 = *off
016600000801     C                             and
016700000801     C                             (vinflg = *blanks
016800000801     C                              or vinflg = '0'
016900000801     C                              or vinflg = '2')
017000000801     C*
017100000801     C                   clear                   vinmsg
017200141028     C*
017300141028     C* "normalizzo" il dato di input
017400141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
017500141028     C                                             %len(%trim(vindta))-1))
017600160209     C*
017700160209     C* Elimino le "duple di controllo EDIFACT"
017800160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
017900160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
018000160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018100160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
018200160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
018300040910     C*
018400040910     C* Eseguo routine d traduzione
018500040910     C                   exsr      impvabvat
018600040802     C*
018700010305     C                   endif
018800000905     C*
018900000905     C                   else
019000000905     C                   eval      vinflg = '1'
019100000905     C                   endif
019200000905     C*
019300000905     C  N70              update    tivin000
019400000905     C*
019500991022     C  N70              ENDdo
019600100722     C*
019700100722     C* Scarico i buffer testata ancora "in canna"
019800140922     C*
019900141028     C***  N31              ADD       1             §CTROKVB
020000141028     C***  N31              exsr      CHKWRI
020100141028     C***  N31              WRITE     EDIVAB00
020200010202     C*
020300010202     C                   endif
020400990910
020500990910     C* Se non ci sono record con errori ...
020600000710     C                   if        §ctrno = 0
020700990910     C* ... restituisco esito OK.
020800990921     C                   eval      wrkesito = '0'
020900990910     C                   else
021000010201     C                   if        §ctrokvb > 0
021100990921     C                   eval      wrkesito = '1'
021200000710     C                   else
021300000710     C                   eval      wrkesito = '2'
021400990910     C                   endif
021500000710     C                   endif
021600990910     C*
021700990914     C                   if        %open(tivin00r)
021800990908     C                   close     tivin00r
021900990914     C                   endif
022000140117     C                   if        %open(edivabwr)
022100140117     C                   close     edivabwr
022200990914     C                   endif
022300140117     C                   if        %open(edivatwr)
022400140117     C                   close     edivatwr
022500010201     C                   endif
022600990910     C*
022700010201     C                   if        §ctrokvb > 0
022800000724     C                             and vlrpoi <> *zeros
022900010202     C                   exsr      invio
023000990920     C                   endif
023100990920     C*
023200910830     C                   ENDSR
023300000613     C***
023400140922
023500140922     C*----------------------------------------------------*
023600140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
023700140922     C*----------------------------------------------------*
023800140922     C     CHKWRI        BEGSR
023900140922     C*
024000141028     C                   if        VABRMN = *zeros
024100141028     C                   eval      VABRMN = VABNSP
024200141028     C                   endif
024300141028     C*
024400141028     C                   if        VABPRD = *blanks
024500141028     C* ......VABPRD
024600141028     C* Reperisco la provincia dal CAP e dalla località
024700141028     C                   IF        VABLOD <> *blanks AND
024800141028     C                             VABCAD <> *blanks AND
024900141028     C                             VABNZD  = *blanks
025000141028     C                   CLEAR                   TISI95DS
025100141028     C                   EVAL      I95TCN = '3'
025200141028     C                   Z-ADD     datcor        I95DAT
025300141028     C                   EVAL      I95CAP = VABCAD
025400141028     C                   EVAL      I95LOC = VABLOD
025500141028     C                   CALL      'TISI95R'
025600141028     C                   PARM                    TISI95DS
025700141028     C                   EVAL      VABPRD = O95PRV
025800141028     C                   ENDIF
025900141028     C                   endif
026000140922     C*
026100140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
026200141028     C                   IF        wFlgCAS <> '0'
026300140922     C                   IF        VABCBO = '1'
026400140922     C                   EVAL      VABCBO = '4'
026500140922     C                   ELSE
026600140922     C                   EVAL      VABCBO = '6'
026700140922     C                   ENDIF
026800140922     C                   ENDIF
026900140922     C*
027000140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027100141028     C                   EXSR      CHKIMPDIV
027200150527     C*
027300150527     C* Scompongo le note
027400161206     C                   EVAL      wNOTE = %trim(wNOTE)   + ' ' +
027500161206     C                                     %trim(wNOTE_2)
027600161206     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
027700161206     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
027800170201     C*
027900170201     C* Codice cliente mittente
028000170201     C                   if        savCCM > *zeros
028100170201     C                   eval      VABCCM = savCCM
028200170201     C                   endif
028300140922     C*
028400140922     C                   ENDSR
028500141028     C*----------------------------------------------------*
028600141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700141028     C*----------------------------------------------------*
028800141028     C     INZVAR        BEGSR
028900141028     C*
029000141028     C* Inizializzo variabili di wrk
029100141028     C                   Z-ADD     *zeros        Num5_0            5 0
029200141028     C                   MOVEL     '0'           wFlgCAS
029300141028     C                   MOVEL     *blanks       wVATNOT_A
029400141028     C                   MOVEL     *blanks       wVATNOT_B
029500141028     C                   MOVEL     *blanks       wVATNOT_E
029600141028     C                   MOVEL     *blanks       wVATNOT_I
029700141028     C                   MOVEL     *blanks       wVATNOT_J
029800141028     C                   MOVEL     *blanks       wVATNOT_S
029900141028     C                   MOVEL     *blanks       wVATNOT_P
030000141028     C                   CLEAR                   SkSplitCSV_5
030100150527     C                   MOVEL     *blanks       wNOTE
030200161206     C                   MOVEL     *blanks       wNOTE_2
030300141028     C*
030400141028     C* Reimposto i valori di default
030500141028     C                   EXSR      DEFCAM
030600141028     C*
030700141028     C                   ENDSR
030800141028     C*----------------------------------------------------*
030900141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
031000141028     C*----------------------------------------------------*
031100141028     C     DEFCAM        BEGSR
031200141028     C*
031300141028     C                   CLEAR                   EDIVAB00
031400141028     C                   CLEAR                   EDIVAT00
031500141028     C*
031600141028     C* Imposto i valori bolla di default
031700161206     C                   EVAL      VABCTR = 000
031800161206     C                   EVAL      VABCBO = '1'
031900161206     C                   EVAL      VABTSP = 'C'
032000161206     C*
032100141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032200141028     C* e delimitatore testo.
032300141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032400141028     C*
032500141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
032600141028     C                   EVAL      CharSOS = CharNUM
032700141028     C*
032800141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
032900141028     C                   SETOFF                                       53
033000141028     C                   SELECT
033100141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
033200141028     C                   SETON                                        53
033300141028     C                   ENDSL
033400141028     C*
033500141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
033600141028     C                   EVAL      posDaDft = 1
033700141028     C                   EVAL      posADft  = 0
033800141028     C                   EVAL      wGiroDft = 0
033900141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034000141028     C                             posDaDft > 0
034100141028     C*
034200141028     C* Gestisco il 1° giro
034300141028     C                   IF        wGiroDft = 0
034400141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
034500141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
034600141028     C* Incremento il contatore dei "giri"
034700141028     C                   EVAL      wGiroDft = 1
034800141028     C                   ELSE
034900141028     C                   EVAL      posDaDft = posADft
035000141028     C                   ENDIF
035100141028     C* Eseguo lo scan x trovare la fine del campo corrente
035200141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035300141028     C*
035400141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
035500141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
035600141028     C                   IF        posDaDft > 0 AND
035700141028     C                             posADft  > 0
035800141028     C* NCL
035900141028     C                   IF        %subst(
036000141028     C                             %subst(vlrppt:posDaDft+1:
036100141028     C                             posADft-posDaDft-1):1:3)
036200141028     C                             = 'NCL'
036300141028     C                   EVAL      PiStr=%trim(%subst(
036400141028     C                             %subst(vlrppt:posDaDft+1:
036500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036600141028     C                   EXSR      CHKNUM
036700141028     C                   IF        PiInt=*on
036800141028     C                   Z-ADD     PiVal         VABNCL
036900141028     C                   ENDIF
037000141028     C                   ENDIF
037100141028     C* CCM
037200141028     C                   IF        %subst(
037300141028     C                             %subst(vlrppt:posDaDft+1:
037400141028     C                             posADft-posDaDft-1):1:3)
037500141028     C                             = 'CCM'
037600141028     C                   EVAL      PiStr=%trim(%subst(
037700141028     C                             %subst(vlrppt:posDaDft+1:
037800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037900141028     C                   EXSR      CHKNUM
038000141028     C                   IF        PiInt=*on
038100141028     C                   Z-ADD     PiVal         VABCCM
038200141028     C                   ENDIF
038300141028     C                   ENDIF
038400141028     C* LNP
038500141028     C                   IF        %subst(
038600141028     C                             %subst(vlrppt:posDaDft+1:
038700141028     C                             posADft-posDaDft-1):1:3)
038800141028     C                             = 'LNP'
038900141028     C                   EVAL      PiStr=%trim(%subst(
039000141028     C                             %subst(vlrppt:posDaDft+1:
039100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039200141028     C                   EXSR      CHKNUM
039300141028     C                   IF        PiInt=*on
039400141028     C                   Z-ADD     PiVal         VABLNP
039500141028     C                   ENDIF
039600141028     C                   ENDIF
039700141028     C* NRS
039800141028     C                   IF        %subst(
039900141028     C                             %subst(vlrppt:posDaDft+1:
040000141028     C                             posADft-posDaDft-1):1:3)
040100141028     C                             = 'NRS'
040200141028     C                   EVAL      PiStr=%trim(%subst(
040300141028     C                             %subst(vlrppt:posDaDft+1:
040400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040500141028     C                   EXSR      CHKNUM
040600141028     C                   IF        PiInt=*on
040700141028     C                   Z-ADD     PiVal         VABNRS
040800141028     C                   ENDIF
040900141028     C                   ENDIF
041000141028     C* CTR
041100141028     C                   IF        %subst(
041200141028     C                             %subst(vlrppt:posDaDft+1:
041300141028     C                             posADft-posDaDft-1):1:3)
041400141028     C                             = 'CTR'
041500141028     C                   EVAL      PiStr=%trim(%subst(
041600141028     C                             %subst(vlrppt:posDaDft+1:
041700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041800141028     C                   EXSR      CHKNUM
041900141028     C                   IF        PiInt=*on
042000141028     C                   Z-ADD     PiVal         VABCTR
042100141028     C                   ENDIF
042200141028     C                   ENDIF
042300141028     C* PKB
042400141028     C                   IF        %subst(
042500141028     C                             %subst(vlrppt:posDaDft+1:
042600141028     C                             posADft-posDaDft-1):1:3)
042700141028     C                             = 'PKB'
042800141028     C                   EVAL      PiStr=%trim(%subst(
042900141028     C                             %subst(vlrppt:posDaDft+1:
043000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043100141028     C                   EXSR      CHKNUM
043200141028     C                   IF        PiNum=*on
043300141028     C                   Z-ADD     PiVal         VABPKB
043400141028     C                   ENDIF
043500141028     C                   ENDIF
043600141028     C* VLB
043700141028     C                   IF        %subst(
043800141028     C                             %subst(vlrppt:posDaDft+1:
043900141028     C                             posADft-posDaDft-1):1:3)
044000141028     C                             = 'VLB'
044100141028     C                   EVAL      PiStr=%trim(%subst(
044200141028     C                             %subst(vlrppt:posDaDft+1:
044300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044400141028     C                   EXSR      CHKNUM
044500141028     C                   IF        PiNum=*on
044600141028     C                   Z-ADD     PiVal         VABVLB
044700141028     C                   ENDIF
044800141028     C                   ENDIF
044900141028     C* QFT
045000141028     C                   IF        %subst(
045100141028     C                             %subst(vlrppt:posDaDft+1:
045200141028     C                             posADft-posDaDft-1):1:3)
045300141028     C                             = 'QFT'
045400141028     C                   EVAL      PiStr=%trim(%subst(
045500141028     C                             %subst(vlrppt:posDaDft+1:
045600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045700141028     C                   EXSR      CHKNUM
045800141028     C                   IF        PiNum=*on
045900141028     C                   Z-ADD     PiVal         VABQFT
046000141028     C                   ENDIF
046100141028     C                   ENDIF
046200141028     C* CBO
046300141028     C                   IF        %subst(
046400141028     C                             %subst(vlrppt:posDaDft+1:
046500141028     C                             posADft-posDaDft-1):1:3)
046600141028     C                             = 'CBO'
046700141028     C                   EVAL      VABCBO=%trim(%subst(
046800141028     C                             %subst(vlrppt:posDaDft+1:
046900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047000141028     C                   ENDIF
047100141028     C* TSP
047200141028     C                   IF        %subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):1:3)
047500141028     C                             = 'TSP'
047600141028     C                   EVAL      VABTSP=%trim(%subst(
047700141028     C                             %subst(vlrppt:posDaDft+1:
047800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047900141028     C                   ENDIF
048000141028     C* VAS
048100141028     C                   IF        %subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):1:3)
048400141028     C                             = 'VAS'
048500141028     C                   EVAL      VABVAS=%trim(%subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800141028     C                   ENDIF
048900141028     C* VCA
049000141028     C                   IF        %subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):1:3)
049300141028     C                             = 'VCA'
049400141028     C                   EVAL      VABVCA=%trim(%subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700141028     C                   ENDIF
049800141028     C* TIC
049900141028     C                   IF        %subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):1:3)
050200141028     C                             = 'TIC'
050300141028     C                   EVAL      VABTIC=%trim(%subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600141028     C                   ENDIF
050700141028     C* GCA
050800141028     C                   IF        %subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):1:3)
051100141028     C                             = 'GCA'
051200141028     C                   EVAL      VABGCA=%trim(%subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500141028     C                   ENDIF
051600141028     C* CTM
051700141028     C                   IF        %subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):1:3)
052000141028     C                             = 'CTM'
052100141028     C                   EVAL      VABCTM=%trim(%subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400141028     C                   ENDIF
052500141028     C* FFD
052600141028     C                   IF        %subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):1:3)
052900141028     C                             = 'FFD'
053000141028     C                   EVAL      VABFFD=%trim(%subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300141028     C                   ENDIF
053400141028     C* VAD
053500141028     C                   IF        %subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):1:3)
053800141028     C                             = 'VAD'
053900141028     C                   EVAL      VABVAD=%trim(%subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200141028     C                   ENDIF
054300141028     C* GMA
054400141028     C                   IF        %subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):1:3)
054700141028     C                             = 'GMA'
054800141028     C                   EVAL      VABGMA=%trim(%subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100141028     C                   ENDIF
055200141028     C* GGA
055300141028     C                   IF        %subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):1:3)
055600141028     C                             = 'GGA'
055700141028     C                   EVAL      VABGGA=%trim(%subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000141028     C                   ENDIF
056100141028     C* GVA
056200141028     C                   IF        %subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):1:3)
056500141028     C                             = 'GVA'
056600141028     C                   EVAL      VABGVA=%trim(%subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900141028     C                   ENDIF
057000141028     C* TC1
057100141028     C                   IF        %subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):1:3)
057400141028     C                             = 'TC1'
057500141028     C                   EVAL      VABTC1=%trim(%subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800141028     C                   ENDIF
057900141028     C* TC2
058000141028     C                   IF        %subst(
058100141028     C                             %subst(vlrppt:posDaDft+1:
058200141028     C                             posADft-posDaDft-1):1:3)
058300141028     C                             = 'TC2'
058400141028     C                   EVAL      VABTC2=%trim(%subst(
058500141028     C                             %subst(vlrppt:posDaDft+1:
058600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700141028     C                   ENDIF
058800141028     C* VATTRC
058900141028     C                   IF        %subst(
059000141028     C                             %subst(vlrppt:posDaDft+1:
059100141028     C                             posADft-posDaDft-1):1:3)
059200141028     C                             = 'TRC'
059300141028     C                   EVAL      VATTRC=%trim(%subst(
059400141028     C                             %subst(vlrppt:posDaDft+1:
059500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600141028     C                   ENDIF
059700141028     C* ...
059800141028     C                   ENDIF
059900141028     C                   ENDDO
060000141028     C*
060100141028     C                   ENDSR
060200000801     C*----------------------------------------------------*
060300140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
060400000801     C*----------------------------------------------------*
060500040910     C     IMPVABVAT     BEGSR
060600040910     C*
060700040910     C* Traduzione relativa ai tipi record del file del cliente
060800040910     C*
060900071210     C*
061000071210     C***
061100141028     C* ...tipo record 'UNB' (info CMR)
061200141028     C                   EVAL      wTag = 'UNB'
061300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
061400161206     C                   MOVEL     *blanks       savCMR_1         35
061500141028     C                   EVAL      InStringa =
061600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
061700141028     C                   EVAL      InSepara = '+'
061800141028     C                   EXSR      SR_SPLIT
061900161206     C                   EVAL      savCMR_1 = %trim(SkSplitCSV_5(5)) + ' '+
062000140922     C                                      %char(datcor) + ' ' + %char(oracor)
062100130620     C                   ENDIF
062200161206     C*
062300161206     C***
062400161206     C* ...tipo record 'RFF+ABO' (info CMR)
062500161206     C                   EVAL      wTag = 'RFF+ABO'
062600161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062700161206     C                   MOVEL     *blanks       savCMR_2         35
062800161206     C                   EVAL      InStringa =
062900161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
063000161206     C                   EVAL      InSepara = '+'
063100161206     C                   EXSR      SR_SPLIT
063200161206     C                   EVAL      savCMR_2 = %trim(SkSplitCSV_5(1)) + ' '+
063300161206     C                                      %char(datcor) + ' ' + %char(oracor)
063400161206     C                   ENDIF
063500141028     C*
063600141028     C***
063700170313     C* ...tipo record 'BGM+787' (rottuta di codice spedizione)
063800170313     C                   EVAL      wTag = 'BGM+787'
063900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064000150526     C*
064100150526     C* Scarico buffer spedizione elaborata
064200150526     C                   IF        *IN90
064300150526     C  N31              ADD       1             §CTROKVB
064400150526     C  N31              exsr      CHKWRI
064500150526     C  N31              WRITE     EDIVAB00
064600150526     C                   ENDIF
064700150526     C                   SETON                                        90
064800150526     C*
064900150526     C* Inizializzazioni per nuova spedizione
065000141028     C                   SETOFF                                       31
065100141028     C                   eval      vinflg = '1'
065200141028     C* ......inizializzazioni iniziali e formati record file Bartolini
065300141028     C                   EXSR      INZVAR
065400141028     C* ......valorizzazione campi fissi
065500141028     C                   MOVEL     datcor        VABAAS
065600141028     C                   MOVE      datcor        VABMGS
065700141028     C                   MOVE(P)   vlrpoi        VABFGS
065800161206     C                   EVAL      VABCMR = savCMR_1 + ' - ' + savCMR_2
065900141028     C                   EVAL      VABDCM = datcor
066000141028     C                   EVAL      VABDTS = datcor
066100141028     C                   EVAL      VABCNT = 1
066200141028     C* ......NSP => Stacco un numeratore da AZNUM
066300141028     C                   clear                   TRUL33DS
066400141028     C                   eval      I33OPE = *zeros
066500141028     C                   eval      I33CNU = 302
066600141028     C                   eval      I33NUM = 1
066700141028     C                   movel     TRUL33DS      KPJBU
066800141028     C                   call      'TRUL33R'
066900141028     C                   parm                    KPJBA
067000141028     C                   movel     KPJBU         TRUL33DS
067100141028     C                   if        O33ERR = *zeros
067200141028     C                   z-add     O33NRF        VABNSP
067300141028     C                   else
067400141028     C                   Z-ADD     1             wRecErr
067500141028     C                   SETON                                        31
067600141028     C                   EVAL      vinmsg = %trimr(vinmsg)
067700141028     C                             + ' ' + 'VABNSP VATNSP'
067800141028     C                   endif
067900150811     C*
068000150811     C***
068100150811     C* ...tipo record 'CNI' (riferimento spedizione)
068200170303     C***                EVAL      InStringa =
068300170303     C***                             %subst(wvindta:%len(%trim(wTag))+2+2)
068400170303     C***                EVAL      InSepara = ':'
068500170303     C***                EXSR      SR_SPLIT
068600170303     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
068700170303     C***                EXSR      CHKNUM
068800170303     C***                IF        PiNum=*on
068900170303     C***                Z-ADD     PiVal         VABRMN
069000170303     C***                ELSE
069100170303     C***                Z-ADD     1             VABRMN
069200170303     C***                ENDIF
069300150811     C*
069400141028     C                   ENDIF
069500170303     C*
069600170303     C***
069700170303     C* ...tipo record 'RFF+ACL' (riferimento spedizione)
069800170303     C                   EVAL      wTag = 'RFF+ACL'
069900170303     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
070000170303     C                   EVAL      PiStr=
070100170303     C                                %subst(wvindta:%len(%trim(wTag))+2+2)
070200170303     C                   EXSR      CHKNUM
070300170303     C                   IF        PiNum=*on
070400170303     C                   Z-ADD     PiVal         VABRMN
070500170303     C                   ELSE
070600170303     C                   Z-ADD     1             VABRMN
070700170303     C                   ENDIF
070800170303     C                   ENDIF
070900161206     C*
071000161206     C***
071100161206     C* ...tipo record 'DTM+2' (data consegna richiesta)
071200161206     C                   EVAL      wTag = 'DTM+2'
071300161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071400161206     C                   EVAL      PiStr=%subst(
071500161206     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
071600161206     C                   EXSR      CHKNUM
071700161206     C                   IF        PiInt=*on
071800161206     C                   Z-ADD     PiVal         VABDCR
071900161206     C                   ELSE
072000161206     C                   Z-ADD     1             wRecErr
072100161206     C                   ENDIF
072200161206     C                   ENDIF
072300161206     C*
072400161206     C***
072500161206     C* ...tipo record 'TSR+' (tipo servizio)
072600161206     C                   EVAL      wTag = 'TSR+'
072700161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072800161206     C                   EVAL      InStringa =
072900161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
073000161206     C                   EVAL      InSepara = '+'
073100161206     C                   EXSR      SR_SPLIT
073200161206     C                   IF        %trim(SkSplitCSV_5(3)) <> '8'
073300161206     C                   EVAL      VABDCR = *zeros
073400161206     C                   ENDIF
073500161206     C                   IF        %trim(SkSplitCSV_5(3)) = '6'
073600161206     C                   EVAL      VABTC1 = 'A'
073700161206     C                   ENDIF
073800161206     C                   ENDIF
073900161206     C*
074000161206     C***
074100161206     C* ...tipo record 'RFF+ON' (riferimento spedizione)
074200161206     C                   EVAL      wTag = 'RFF+ON'
074300161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074400161206     C                   EVAL      VABRMA =
074500161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
074600161206     C                   ENDIF
074700161223     C*
074800161223     C***
074900161223     C* ...tipo record 'NAD+CZ' (mittente)
075000161223     C                   EVAL      wTag = 'NAD+CZ'
075100161223     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075200161223     C                   EVAL      InStringa =
075300161223     C                                %subst(wvindta:%len(%trim(wTag))+2)
075400161223     C                   EVAL      InSepara = '+'
075500161223     C                   EXSR      SR_SPLIT
075600161223     C                   EVAL      InStringa = %trim(SkSplitCSV_5(1))
075700161223     C                   EVAL      InSepara = ':'
075800161223     C                   EXSR      SR_SPLIT
075900170201     C                   Z-ADD     *zeros        savCCM            7 0
076000161223     C                   SELECT
076100161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '7777700000007'
076200170201     C                   EVAL      savCCM = 0055072
076300161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '5414321000000'
076400170201     C                   EVAL      savCCM = 0055068
076500161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '5414522000007'
076600170201     C                   EVAL      savCCM = 0055074
076700161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '5413405000004'
076800170201     C                   EVAL      savCCM = 0055075
076900161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '5411097000005'
077000170201     C                   EVAL      savCCM = 0055069
077100161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '7157520000003'
077200170201     C                   EVAL      savCCM = 0055069
077300161223     C                   WHEN      %trim(SkSplitCSV_5(1)) = '5017815000007'
077400170201     C                   EVAL      savCCM = 0057952
077500161223     C                   ENDSL
077600161223     C                   ENDIF
077700161206     C*
077800161206     C***
077900161206     C* ...tipo record 'NAD+CN' (destinatario)
078000161206     C                   EVAL      wTag = 'NAD+CN'
078100161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078200161206     C                   EVAL      InStringa =
078300161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
078400161206     C                   EVAL      InSepara = '+'
078500161206     C                   EXSR      SR_SPLIT
078600161206     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(3))
078700161206     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
078800161206     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
078900161206     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
079000161206     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
079100161206     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
079200161206     C                   IF        VABNZD = 'I'   OR
079300161206     C                             VABNZD = 'IT'  OR
079400161206     C                             VABNZD = 'ITA'
079500161206     C                   EVAL      VABNZD = *blanks
079600161206     C                   ENDIF
079700161206     C                   ENDIF
079800161206     C*
079900161206     C***
080000161206     C* ...tipo record 'CTA+CN (referente consegna)
080100161206     C                   EVAL      wTag = 'CTA+CN'
080200161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080300161206     C                   EVAL      InStringa =
080400161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
080500161206     C                   EVAL      InSepara = ':'
080600161206     C                   EXSR      SR_SPLIT
080700161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
080800161206     C                   EVAL      VATTRC = 'A'
080900161206     C                   EXSR      WRIVAT
081000161206     C                   ENDIF
081100161206     C*
081200161206     C***
081300161206     C* ...tipo record 'COM' (numero telefono referente consegna / email destinatario)
081400161206     C                   EVAL      wTag = 'COM'
081500161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081600161206     C                   EVAL      InStringa =
081700161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
081800161206     C                   EVAL      InSepara = ':'
081900161206     C                   EXSR      SR_SPLIT
082000161206     C                   IF        %trim(SkSplitCSV_5(2)) = 'TE'
082100161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
082200161206     C                   EVAL      VATTRC = 'B'
082300161206     C                   EXSR      WRIVAT
082400161206     C                   ENDIF
082500161206     C                   IF        %trim(SkSplitCSV_5(2)) = 'EM'
082600161206     C                   EVAL      VATNOT = %subst(SkSplitCSV_5(1):1:35)
082700161206     C                   EVAL      VATTRC = 'I'
082800161206     C                   EXSR      WRIVAT
082900161206     C                   EVAL      VATNOT = %subst(SkSplitCSV_5(1):36:35)
083000161206     C                   EVAL      VATTRC = 'J'
083100161206     C                   EXSR      WRIVAT
083200161206     C                   ENDIF
083300161206     C                   ENDIF
083400161206     C*
083500161206     C***
083600161206     C* ...tipo record 'HAN+HWC' (note di consegna)
083700161206     C                   EVAL      wTag = 'HAN+HWC'
083800161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083900161206     C                   EVAL      InStringa =
084000161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
084100161206     C                   EVAL      InSepara = '+'
084200161206     C                   EXSR      SR_SPLIT
084300161206     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
084400161206     C                                     %trim(SkSplitCSV_5(3))
084500161206     C                   ENDIF
084600161206     C*
084700161206     C***
084800161206     C* ...tipo record 'FTX+AAA' (natura merce)
084900161206     C                   EVAL      wTag = 'FTX+AAA'
085000161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
085100161206     C                   EVAL      InStringa =
085200161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
085300161206     C                   EVAL      InSepara = '+'
085400161206     C                   EXSR      SR_SPLIT
085500161206     C                   EVAL      VABNAS = %trim(SkSplitCSV_5(3))
085600161206     C                   ENDIF
085700161206     C*
085800161206     C***
085900161206     C* ...tipo record 'MEA+WT++' (peso spedizione)
086000161206     C                   EVAL      wTag = 'MEA+WT++'
086100161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086200161206     C                   EVAL      InStringa =
086300161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
086400161206     C                   EVAL      InSepara = ':'
086500161206     C                   EXSR      SR_SPLIT
086600161206     C                   EVAL      PiStr=%trim(SkSplitCSV_5(2))
086700161206     C                   EXSR      CHKNUM
086800161206     C                   IF        PiNum=*on
086900161206     C                   ADD       PiVal         VABPKB
087000161206     C                   ELSE
087100161206     C                   Z-ADD     1             wRecErr
087200161206     C                   ENDIF
087300161206     C                   ENDIF
087400161206     C*
087500141028     C***
087600161206     C* ...tipo record 'RFF+AAS' (riferimento spedizione)
087700161206     C                   EVAL      wTag = 'RFF+AAS'
087800161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
087900161206     C                   EVAL      VABRMA =
088000161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
088100161206     C                   ENDIF
088200170303     C*
088300170303     C***
088400170303     C* ...tipo record 'RFF+AAO' (riferimento spedizione)
088500170303     C                   EVAL      wTag = 'RFF+AAO'
088600170303     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088700170303     C                   EVAL      VABRMA =
088800170303     C                                %subst(wvindta:%len(%trim(wTag))+2)
088900170303     C                   ENDIF
089000150527     C*
089100150527     C***
089200161206     C* ...tipo record 'FTX+DEL' (note di consegna aggiuntive)
089300161206     C                   EVAL      wTag = 'FTX+DEL'
089400150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089500150527     C                   EVAL      InStringa =
089600150527     C                                %subst(wvindta:%len(%trim(wTag))+2)
089700150527     C                   EVAL      InSepara = '+'
089800150527     C                   EXSR      SR_SPLIT
089900161206     C                   EVAL      wNOTE_2 = %trim(wNOTE_2) + ' ' +
090000161206     C                                       %trim(SkSplitCSV_5(3))
090100161206     C                   ENDIF
090200161206     C*
090300161206     C***
090400161206     C* ...tipo record 'PCI+18' (dettagli segnacolli)
090500161206     C                   EVAL      wTag = 'PCI+18'
090600161206     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090700161206     C                   EVAL      InStringa =
090800161206     C                                %subst(wvindta:%len(%trim(wTag))+2)
090900161206     C                   EVAL      InSepara = '+'
091000161206     C                   EXSR      SR_SPLIT
091100161206     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
091200161206     C                   ADD       1             VABNCL
091300161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
091400161206     C                   EVAL      VATTRC = 'E'
091500161206     C                   EXSR      WRIVAT
091600161206     C                   ENDIF
091700161206     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
091800161206     C                   ADD       1             VABNCL
091900161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
092000161206     C                   EVAL      VATTRC = 'E'
092100161206     C                   EXSR      WRIVAT
092200161206     C                   ENDIF
092300161206     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
092400161206     C                   ADD       1             VABNCL
092500161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
092600161206     C                   EVAL      VATTRC = 'E'
092700161206     C                   EXSR      WRIVAT
092800161206     C                   ENDIF
092900161206     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
093000161206     C                   ADD       1             VABNCL
093100161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
093200161206     C                   EVAL      VATTRC = 'E'
093300161206     C                   EXSR      WRIVAT
093400161206     C                   ENDIF
093500161206     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
093600161206     C                   ADD       1             VABNCL
093700161206     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
093800161206     C                   EVAL      VATTRC = 'E'
093900161206     C                   EXSR      WRIVAT
094000161206     C                   ENDIF
094100161206     C                   ENDIF
094200141028     C*
094300141028     C***
094400141028     C* ...tipo record '???' (PDF per packing-list)
094500141028     C                   EVAL      wTag = '???PDF'
094600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
094700141028     C*
094800141028     C* ......VATNOT_P
094900141028     C                   if        *in53
095000141028     C*
095100141028     C* Solo se valorizzato RMN
095200141028     C                   if        VABRMA <> *blanks
095300141028     C                   eval      pIn_MaskPDF =
095400141028     C                                     %trim(%subst(wvindta:1292:100))
095500141028     C                   call      'TIS7P2SER'
095600141028     C                   parm      'W'           pIn_Opz           1
095700141028     C                   parm                    tivlrds
095800141028     C                   parm                    EDIVABDS
095900141028     C                   parm      '10'          pIn_CdIdAz        2
096000141028     C                   parm                    pIn_MaskPDF      50
096100141028     C                   parm      *blanks       pOut_Esito        1
096200141028     C                   endif
096300141028     C                   endif
096400141028     C*
096500141028     C                   ENDIF
096600141028     C*
096700141028     C***
096800141028     C* ...tipo record 'UNT' (fine bolla)
096900141028     C                   EVAL      wTag = 'UNT'
097000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097100150526     C                   IF        *IN90
097200141028     C  N31              ADD       1             §CTROKVB
097300141028     C  N31              exsr      CHKWRI
097400140117     C  N31              WRITE     EDIVAB00
097500150526     C                   ENDIF
097600150526     C                   ENDIF
097700010202     C*
097800000801     C* Ebbene...
097900000801     C                   ADD       1             §CTRMO
098000141028     C                   IF        wRecErr <> *zeros
098100000801     C                   ADD       1             §CTRNO
098200000801     C                   EVAL      vinflg = '2'
098300000801     C                   ENDIF
098400000801     C*
098500000801     C                   ENDSR
098600010202     C*----------------------------------------------------*
098700140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
098800010202     C*----------------------------------------------------*
098900020305     C     PREVAT        BEGSR
099000010202     C*
099100140117     C* Compongo il nome del membro da dare al EDIVATWR
099200010202     C                   eval      parmbr = vlrhdl
099300010202     C                   movel     'M'           parmbr
099400060113     C                   eval      parccm = vlrksc
099500010202     C                   eval      paropz = '1'
099600010202     C* Effettuo la chiamata al CLLE preposto
099700140117     C                   call(e)   'TITVEVTC'
099800010202     C                   parm                    parccm
099900010202     C                   parm                    parmbr
100000010202     C                   parm                    paropz
100100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
100200010202     C                   if        %error
100300010202     C                   movel     '1'           chkcall
100400010202     C                   else
100500010202     C                   movel     '0'           chkcall
100600010202     C                   endif
100700010202     C*
100800010202     C                   ENDSR
100900141028     C*----------------------------------------------------*
101000141028     C*  CONTROLLO NUMERICITA' CAMPI
101100141028     C*----------------------------------------------------*
101200141028     C     CHKNUM        BEGSR
101300141028     C*
101400141028     C                   IF        PiDecChr = *blanks
101500141028     C                   EVAL      PiDecChr = CharNUM
101600141028     C                   ENDIF
101700141028     C*
101800141028     C                   callp     UBISNUM_Check(PiStr
101900141028     C                                          :PiDecChr
102000141028     C                                          :PiVal
102100141028     C                                          :PiNum
102200141028     C                                          :PiInt)
102300141028     C*
102400141028     C                   ENDSR
102500141028     C***
102600141028
102700141028
102800141028     C*----------------------------------------------------*
102900141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
103000141028     C*----------------------------------------------------*
103100141028     C     SR_SPLIT      BEGSR
103200141028     C*
103300141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
103400141028     C                   CALL      'XSPLIT2'
103500141028     C                   PARM                    InStringa
103600141028     C                   PARM                    InSepara
103700141028     C                   PARM      '5'           InTypeOut
103800141028     C                   PARM      ''            wSkParam
103900141028     C                   PARM                    OutErrore
104000141028     C                   PARM                    OutMsg
104100141028     C                   MOVEA     wSkParam      SkSplitCSV_5
104200141028     C*
104300141028     C                   ENDSR
104400141028
104500141028
104600141028     C*----------------------------------------------------*
104700141028     C*  SCRITTURA BUFFER EDIVAT
104800141028     C*----------------------------------------------------*
104900141028     C     WRIVAT        BEGSR
105000170201     C*
105100170201     C* Codice cliente mittente
105200170201     C                   if        savCCM > *zeros
105300170201     C                   eval      VATCCM = savCCM
105400170201     C                   endif
105500141028     C*
105600170313     C                   IF        VATNOT <> *blanks AND
105700170313     C                             VATNOT <> 'NA'
105800141028     C                   EVAL      VATFGS = VABFGS
105900141028     C                   EVAL      VATLNP = VABLNP
106000141028     C                   EVAL      VATAAS = VABAAS
106100141028     C                   EVAL      VATNRS = VABNRS
106200141028     C                   EVAL      VATNSP = VABNSP
106300141028     C                   EVAL      VATCMR = VABCMR
106400141028     C                   EVAL      VATCNT = VABCNT
106500141028     C                   ADD       1             §CTROKVT
106600141028     C                   WRITE     EDIVAT00
106700141028     C                   ENDIF
106800141028     C*
106900141028     C                   ENDSR
107000000801
107100011113
107200011113     C*----------------------------------------------------*
107300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
107400011113     C*----------------------------------------------------*
107500011113     C     CHKIMPDIV     BEGSR
107600011113     C*
107700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
107800011113     C                   Z-ADD     *zeros        wrkDec            9 9
107900011113     C*
108000011113     C* Come prima cosa effettuo considerazioni sulla divisa
108100011113     C                   IF        vabIAS > *zeros
108200011113     C                   IF        vabVAS <> 'EUR'
108300011113     C                   EVAL      vabVAS =  'ITL'
108400011113     C                   ENDIF
108500011113     C                   ENDIF
108600011113     C*
108700011113     C                   IF        vabCAS > *zeros
108800011113     C                   IF        vabVCA <> 'EUR'
108900011113     C                   EVAL      vabVCA =  'ITL'
109000011113     C                   ENDIF
109100011113     C                   ENDIF
109200011113     C*
109300011113     C                   IF        vabVMD > *zeros
109400020305     C                   IF        vabVAD <> 'EUR'
109500011113     C                   EVAL      vabVAD =  'ITL'
109600011113     C                   ENDIF
109700011113     C                   ENDIF
109800011113     C*
109900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
110000011113     C                   Z-ADD     vabIAS        wrkDec
110100011113     C                   IF        wrkDec > *zeros
110200011113     C                   IF        vabVAS = 'ITL'
110300011113     C                   EVAL      vabIAS = *zeros
110400011113     C                   ENDIF
110500011113     C                   ENDIF
110600011113     C*
110700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
110800011113     C                   Z-ADD     vabCAS        wrkDec
110900011113     C                   IF        wrkDec > *zeros
111000011113     C                   IF        vabVCA = 'ITL'
111100011113     C                   EVAL      vabCAS = *zeros
111200011113     C                   ENDIF
111300011113     C                   ENDIF
111400011113     C*
111500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
111600011113     C                   Z-ADD     vabVMD        wrkDec
111700011113     C                   IF        wrkDec > *zeros
111800011113     C                   IF        vabVAD = 'ITL'
111900011113     C                   EVAL      vabVMD = *zeros
112000011113     C                   ENDIF
112100011113     C                   ENDIF
112200011113     C*
112300011113     C                   ENDSR
112400011113     C***
112500011113
112600011113
112700000801
112800000801
112900990920      /TITLE Invio dei dati al punto operativo.
113000010202     C     invio         BEGSR
113100990920     C*
113200140117     C* 1° invio EDIVAT
113300010201     C                   reset                   dscmz
113400010201     C                   move      vlrpoi        cmzdst
113500140117     C                   eval      cmzfld = 'EDIVATWR'
113600010201     C                   eval      cmzmbd = vlrhdl
113700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
113800021009     C***                if        prmfir = *blanks
113900140117     C                   eval      cmzfla = 'EDIVAT0F'
114000140117     C                   eval      cmzmba = 'EDIVAT0F'
114100021009     C***                else
114200021009     C***                eval      cmzfla = prmfir
114300021009     C***                eval      cmzmba = prmfir
114400021009     C***                endif
114500010201     C                   eval      cmznrr = *zeros
114600020305     C                   move      §ctrokvt      cmznrr
114700021018     C                   eval      cmzlba = vlrfl1
114800010201     C                   call(e)   'TIS711C'
114900010201     C                   parm                    dscmz
115000010201     C                   parm      *blanks       esito
115100010205     C                   if        %error
115200010205     C                             or cmzerr = '1'
115300010205     C                             or esito  = '1'
115400010205     C                   eval      wrkesito = '3'
115500010205     C                   else
115600010201     C*
115700140117     C* 2° invio EDIVAB
115800010201     C                   reset                   dscmz
115900010201     C                   move      vlrpoi        cmzdst
116000010201     C                   eval      cmzfld = vlrfou
116100010201     C                   eval      cmzmbd = vlrhdl
116200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
116300021009     C***                if        prmfir = *blanks
116400140117     C                   eval      cmzfla = 'EDIVAB0F'
116500140117     C                   eval      cmzmba = 'EDIVAB0F'
116600021009     C***                else
116700021009     C***                eval      cmzfla = prmfir
116800021009     C***                eval      cmzmba = prmfir
116900021009     C***                endif
117000010201     C                   eval      cmznrr = *zeros
117100010201     C                   move      §ctrokvb      cmznrr
117200021018     C                   eval      cmzlba = vlrfl1
117300010201     C                   call(e)   'TIS711C'
117400010201     C                   parm                    dscmz
117500010201     C                   parm      *blanks       esito
117600010201     C                   if        %error
117700010201     C                             or cmzerr = '1'
117800010201     C                             or esito  = '1'
117900010201     C                   eval      wrkesito = '3'
118000010201     C                   endif
118100010205     C                   endif
118200990920     C*
118300000613     C                   ENDSR
118400000613     C***
118500070411
118600141028
118700070411     C     *pssr         BEGSR
118800070411     C*
118900070411     C                   if        %open(tivin00r)
119000070411     C                   close     tivin00r
119100070411     C                   endif
119200140117     C                   if        %open(edivabwr)
119300140117     C                   close     edivabwr
119400070411     C                   endif
119500140117     C                   if        %open(edivatwr)
119600140117     C                   close     edivatwr
119700070411     C                   endif
119800070411     C*
119900070411     C* Effettuo la chiamata al CLLE preposto
120000140117     C                   call(e)   'TITVEVTC'
120100070411     C                   parm                    parccm
120200070411     C                   parm                    parmbr
120300070411     C                   parm      '2'           paropz
120400070411     C*
120500070411     C                   eval      wrkesito = '2'
120600070411     C*
120700070411     C                   seton                                        LR
120800070411     C*
120900070411     C                   ENDSR     '*CANCL'
121000070411     C***
121100070411
121200990910
121300000613     C     *inzsr        BEGSR
121400990910     C*
121500990910     C     *entry        plist
121600990920     C                   parm                    tivlrds
121700990921     C                   parm      wrkesito      esito
121800000724     C                   parm                    prmlit
121900000710     C                   parm                    prmfir
122000000613     C*
122100000830     C* CALCOLA LA DATA CORRENTE
122200140117     C                   time                    wn14             14 0
122300140117     C                   movel     wn14          oracor            6 0          *ORA
122400100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
122500100722     C                   eval      datcor = %dec(%date() : *ISO)
122600000830     C*
122700000613     C                   ENDSR
122800000613     C***
