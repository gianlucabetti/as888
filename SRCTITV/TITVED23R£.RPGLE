000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400161018     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140117     FEDIVABwr  O    E             DISK    usropn
000700140117     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070719     D tisi95ds      e ds
001700140117     D edivabds      e ds                  extname(edivab0f)
001800161018     D ds15          e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800141028     D wTag            s            128    VARYING INZ
002900141028     D wRecErr         s              1  0 INZ(*zeros)
003000141028     D wFlgCAS         s              1    INZ(*blanks)
003100141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003200150527     D wNOTE           s             70    INZ(*blanks)
003300161013     D wEMAIL          s             70    INZ(*blanks)
003400141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003800141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003900141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
004000141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
004100161018
004200161018     D*------------
004300161018     D jNAZ            s              5  0 INZ(*zeros)
004400161018     D skNAZISO        S              3    DIM(1000)
004500161018     D skNAZBAR        S              3    DIM(1000)
004600141028
004700141028     D*------------------
004800141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900141028     D*------------------
005000141028     D CharNUM         S              1
005100141028     D CharSOS         S              1
005200141028     D posDaDft        S              3  0 INZ(*zeros)
005300141028     D posADft         S              3  0 INZ(*zeros)
005400141028     D wGiroDft        s              1  0 INZ(*zeros)
005500141028
005600141028
005700141028     D*------------------
005800141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
005900141028     D*------------------
006000141028     D InStringa       S          65535A   VARYING                              (stringa input)
006100141028     D InSepara        S             10A                                        (separatore)
006200141028     D InTypeOut       S              1A                                        (tipo output)
006300141028     D wSkParam        S          65535A                                        (output)
006400141028     D OutErrore       S              1A                                        (flag errore)
006500141028     D OutMsg          S             80A                                        (messaggio errore)
006600141028     D SkSplitCSV_5    S            256    DIM(256)
006700141028
006800000830
006900041025     D*------------------
007000041025     D* DS REPERIMENTO NUMERATORE
007100041025     D*------------------
007200041025     D trul33ds      e ds                  inz
007300041025     D*------------------
007400041025     D* DS ARCHITETTURA
007500041025     D*------------------
007600041025     D kpjba         e ds                  inz
007700041025     D*------------------
007800990908
007900141028
008000141028     D*------------------
008100141028     D* LINKING A DEFINIZIONI ESTERNE
008200141028     D*------------------
008300141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008400141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008500150609
008600141028
008700150609     D*-------------------
008800150609     D* COSTANTI
008900150609     D*-------------------
009000150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200141028
009300010201
009400010201
009500000913     C                   reset                   rrnum
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800150526     C                   setoff                                       90
009900000613     C*
010000161018     C                   EXSR      CARTAB                                       CARICA TABELLE
010100040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010200070719     C*
010300070719     C* Esegue lancio TISI95R solo x chiusura
010400070719     C                   CLEAR                   TISI95DS
010500070719     C                   EVAL      I95TLA = 'C'
010600070719     C                   CALL      'TISI95R'
010700070719     C                   PARM                    TISI95DS
010800000613     C*
010900010202     C* Effettuo la chiamata al CLLE preposto
011000140922     C                   call(e)   'TITVEVTC'
011100140922     C                   parm                    parccm
011200140922     C                   parm                    parmbr
011300140922     C                   parm      '2'           paropz
011400170731     C*
011500170731     C                   if        *in53
011600170731     C                   call      'TIS7P2SER'
011700170731     C                   parm      'C'           pIn_Opz           1
011800170731     C                   parm                    tivlrds
011900170731     C                   parm                    EDIVABDS
012000170731     C                   parm      *blanks       pIn_CdIdAz        2
012100170731     C                   parm      *blanks       pIn_MaskPDF      50
012200170731     C                   parm      *blanks       pOut_Esito        1
012300170731     C                   endif
012400000616     C*
012500010201     C                   seton                                        LR
012600990908
012700000801
012800910830     C*--------------------------------------------------------
012900140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
013000910830     C*--------------------------------------------------------
013100040526     C     RWFILE        BEGSR
013200990910     C*
013300990914     C                   if        not %open(tivin00r)
013400990908     C                   open      tivin00r
013500990914     C                   endif
013600140117     C                   if        not %open(edivabwr)
013700140117     C                   open      edivabwr
013800990914     C                   endif
013900141028     C*
014000140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014100020305     C                   exsr      prevat
014200010201     C*
014300010202     C                   if        chkcall = '0'
014400010202     C*
014500140117     C                   if        not %open(edivatwr)
014600140117     C                   open      edivatwr
014700010201     C                   endif
014800990910     C*
014900010201     C                   clear                   §CTROKVB          5 0
015000020305     C                   clear                   §CTROKVT          5 0
015100000801     C                   clear                   §CTRMO            5 0
015200000801     C                   clear                   §CTRNO            5 0
015300141028     C*
015400141028     C* Inizializzazioni fuori ciclo
015500141028     C                   EXSR      INZVAR
015600140117     C*
015700140902     C                   if        *in53
015800140117     C                   call      'TIS7P2SER'
015900140117     C                   parm      'O'           pIn_Opz           1
016000140117     C                   parm                    tivlrds
016100140117     C                   parm                    EDIVABDS
016200140117     C                   parm      *blanks       pIn_CdIdAz        2
016300140117     C                   parm      *blanks       pIn_MaskPDF      50
016400140117     C                   parm      *blanks       pOut_Esito        1
016500140902     C                   endif
016600130708     C*
016700921023     C                   DO        *HIVAL
016800990913     C*
016900990915     C                   READ      tivin00r                               70
017000040910     C                   if        vindta > *blanks
017100000613     C                   add       1             rrnum
017200000801     C*
017300000801     C                   if        *in70 = *off
017400000801     C                             and
017500000801     C                             (vinflg = *blanks
017600000801     C                              or vinflg = '0'
017700000801     C                              or vinflg = '2')
017800000801     C*
017900000801     C                   clear                   vinmsg
018000141028     C*
018100141028     C* "normalizzo" il dato di input
018200141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
018300141028     C                                             %len(%trim(vindta))-1))
018400160209     C*
018500160209     C* Elimino le "duple di controllo EDIFACT"
018600160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
018700160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
018800160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018900160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
019000160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
019100040910     C*
019200040910     C* Eseguo routine d traduzione
019300040910     C                   exsr      impvabvat
019400040802     C*
019500010305     C                   endif
019600000905     C*
019700000905     C                   else
019800000905     C                   eval      vinflg = '1'
019900000905     C                   endif
020000000905     C*
020100000905     C  N70              update    tivin000
020200000905     C*
020300991022     C  N70              ENDdo
020400100722     C*
020500100722     C* Scarico i buffer testata ancora "in canna"
020600140922     C*
020700170724     C***  N31              ADD       1             §CTROKVB
020800170724     C***  N31              exsr      CHKWRI
020900170724     C***  N31              WRITE     EDIVAB00
021000010202     C*
021100010202     C                   endif
021200990910
021300990910     C* Se non ci sono record con errori ...
021400000710     C                   if        §ctrno = 0
021500990910     C* ... restituisco esito OK.
021600990921     C                   eval      wrkesito = '0'
021700990910     C                   else
021800010201     C                   if        §ctrokvb > 0
021900990921     C                   eval      wrkesito = '1'
022000000710     C                   else
022100000710     C                   eval      wrkesito = '2'
022200990910     C                   endif
022300000710     C                   endif
022400990910     C*
022500990914     C                   if        %open(tivin00r)
022600990908     C                   close     tivin00r
022700990914     C                   endif
022800140117     C                   if        %open(edivabwr)
022900140117     C                   close     edivabwr
023000990914     C                   endif
023100140117     C                   if        %open(edivatwr)
023200140117     C                   close     edivatwr
023300010201     C                   endif
023400990910     C*
023500010201     C                   if        §ctrokvb > 0
023600000724     C                             and vlrpoi <> *zeros
023700010202     C                   exsr      invio
023800990920     C                   endif
023900990920     C*
024000910830     C                   ENDSR
024100000613     C***
024200140922
024300140922     C*----------------------------------------------------*
024400140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
024500140922     C*----------------------------------------------------*
024600140922     C     CHKWRI        BEGSR
024700140922     C*
024800141028     C                   if        VABRMN = *zeros
024900141028     C                   eval      VABRMN = VABNSP
025000141028     C                   endif
025100141028     C*
025200141028     C                   if        VABPRD = *blanks
025300141028     C* ......VABPRD
025400141028     C* Reperisco la provincia dal CAP e dalla località
025500141028     C                   IF        VABLOD <> *blanks AND
025600141028     C                             VABCAD <> *blanks AND
025700141028     C                             VABNZD  = *blanks
025800141028     C                   CLEAR                   TISI95DS
025900141028     C                   EVAL      I95TCN = '3'
026000141028     C                   Z-ADD     datcor        I95DAT
026100141028     C                   EVAL      I95CAP = VABCAD
026200141028     C                   EVAL      I95LOC = VABLOD
026300141028     C                   CALL      'TISI95R'
026400141028     C                   PARM                    TISI95DS
026500141028     C                   EVAL      VABPRD = O95PRV
026600141028     C                   ENDIF
026700141028     C                   endif
026800140922     C*
026900140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
027000141028     C                   IF        wFlgCAS <> '0'
027100140922     C                   IF        VABCBO = '1'
027200140922     C                   EVAL      VABCBO = '4'
027300140922     C                   ELSE
027400140922     C                   EVAL      VABCBO = '6'
027500140922     C                   ENDIF
027600140922     C                   ENDIF
027700140922     C*
027800140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
027900141028     C                   EXSR      CHKIMPDIV
028000150527     C*
028100150527     C* Scompongo le note
028200150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
028300150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
028400140922     C*
028500140922     C                   ENDSR
028600141028     C*----------------------------------------------------*
028700141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028800141028     C*----------------------------------------------------*
028900141028     C     INZVAR        BEGSR
029000141028     C*
029100141028     C* Inizializzo variabili di wrk
029200141028     C                   Z-ADD     *zeros        Num5_0            5 0
029300141028     C                   MOVEL     '0'           wFlgCAS
029400141028     C                   MOVEL     *blanks       wVATNOT_A
029500141028     C                   MOVEL     *blanks       wVATNOT_B
029600141028     C                   MOVEL     *blanks       wVATNOT_E
029700141028     C                   MOVEL     *blanks       wVATNOT_I
029800141028     C                   MOVEL     *blanks       wVATNOT_J
029900141028     C                   MOVEL     *blanks       wVATNOT_S
030000141028     C                   MOVEL     *blanks       wVATNOT_P
030100141028     C                   CLEAR                   SkSplitCSV_5
030200150527     C                   MOVEL     *blanks       wNOTE
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
031700141028     C                   EVAL      VABCTR = 000
031800141028     C                   EVAL      VABCBO = '1'
031900141104     C                   EVAL      VABTSP = 'C'
032000141028     C*
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
061400141028     C                   MOVEL     *blanks       savCMR           35
061500141028     C                   EVAL      InStringa =
061600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
061700141028     C                   EVAL      InSepara = '+'
061800141028     C                   EXSR      SR_SPLIT
061900141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
062000140922     C                                      %char(datcor) + ' ' + %char(oracor)
062100130620     C                   ENDIF
062200141028     C*
062300141028     C***
062400161025     C* ...tipo record 'UNH' (rottuta di codice spedizione)
062500161025     C                   EVAL      wTag = 'UNH'
062600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
062700150526     C*
062800150526     C* Scarico buffer spedizione elaborata
062900170724     C***                   IF        *IN90
063000170724     C***  N31              ADD       1             §CTROKVB
063100170724     C***  N31              exsr      CHKWRI
063200170724     C***  N31              WRITE     EDIVAB00
063300170724     C***                   ENDIF
063400150526     C                   SETON                                        90
063500150526     C*
063600150526     C* Inizializzazioni per nuova spedizione
063700141028     C                   SETOFF                                       31
063800141028     C                   eval      vinflg = '1'
063900141028     C* ......inizializzazioni iniziali e formati record file Bartolini
064000141028     C                   EXSR      INZVAR
064100141028     C* ......valorizzazione campi fissi
064200141028     C                   MOVEL     datcor        VABAAS
064300141028     C                   MOVE      datcor        VABMGS
064400141028     C                   MOVE(P)   vlrpoi        VABFGS
064500141028     C                   EVAL      VABCMR = savCMR
064600141028     C                   EVAL      VABDCM = datcor
064700141028     C                   EVAL      VABDTS = datcor
064800141028     C                   EVAL      VABCNT = 1
064900141028     C* ......NSP => Stacco un numeratore da AZNUM
065000141028     C                   clear                   TRUL33DS
065100141028     C                   eval      I33OPE = *zeros
065200141028     C                   eval      I33CNU = 302
065300141028     C                   eval      I33NUM = 1
065400141028     C                   movel     TRUL33DS      KPJBU
065500141028     C                   call      'TRUL33R'
065600141028     C                   parm                    KPJBA
065700141028     C                   movel     KPJBU         TRUL33DS
065800141028     C                   if        O33ERR = *zeros
065900141028     C                   z-add     O33NRF        VABNSP
066000141028     C                   else
066100141028     C                   Z-ADD     1             wRecErr
066200141028     C                   SETON                                        31
066300141028     C                   EVAL      vinmsg = %trimr(vinmsg)
066400141028     C                             + ' ' + 'VABNSP VATNSP'
066500141028     C                   endif
066600141028     C                   ENDIF
066700161019     C*
066800161019     C***
066900161025     C* ...tipo record 'UNH' (riferimento spedizione)
067000161025     C                   EVAL      wTag = 'UNH'
067100161019     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067200161019     C                   EVAL      InStringa =
067300161019     C                                %subst(wvindta:%len(%trim(wTag))+2)
067400161025     C                   EVAL      InSepara = '+'
067500161019     C                   EXSR      SR_SPLIT
067600161025     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(1))
067700161019     C                   ENDIF
067800150527     C*
067900150527     C***
068000161025     C* ...tipo record 'DTM+63' (data consegna richiesta)
068100161025     C                   EVAL      wTag = 'DTM+63'
068200150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068300150527     C                   EVAL      PiStr=%subst(
068400150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
068500150527     C                   EXSR      CHKNUM
068600150527     C                   IF        PiInt=*on
068700150527     C                   Z-ADD     PiVal         VABDCR
068800150527     C                   ELSE
068900150527     C                   Z-ADD     1             wRecErr
069000150527     C                   ENDIF
069100150527     C                   ENDIF
069200161025     C*
069300161025     C***
069400161025     C* ...tipo record 'DTM+64' (data consegna richiesta)
069500161025     C                   EVAL      wTag = 'DTM+64'
069600161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069700161025     C                   EVAL      PiStr=%subst(
069800161025     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
069900161025     C                   EXSR      CHKNUM
070000161025     C                   IF        PiInt=*on
070100161025     C                   Z-ADD     PiVal         VABDCR
070200161025     C                   ELSE
070300161025     C                   Z-ADD     1             wRecErr
070400161025     C                   ENDIF
070500161025     C                   ENDIF
070600161025     C*
070700161025     C***
070800161025     C* ...tipo record 'MOA+22' (contrassegno / divisa)
070900161025     C                   EVAL      wTag = 'MOA+22'
071000161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071100161025     C                   EVAL      wFlgCAS = '1'
071200161025     C                   EVAL      InStringa =
071300161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
071400161025     C                   EVAL      InSepara = ':'
071500161025     C                   EXSR      SR_SPLIT
071600161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
071700161025     C                   EXSR      CHKNUM
071800161025     C                   IF        PiNum=*on
071900161025     C                   ADD       PiVal         VABCAS
072000161025     C                   EVAL      VABVCA = %trim(SkSplitCSV_5(2))
072100161025     C                   ELSE
072200161025     C                   Z-ADD     1             wRecErr
072300161025     C                   ENDIF
072400161025     C                   ENDIF
072500161025     C*
072600161025     C***
072700161025     C* ...tipo record 'FTX+SIN' (note di consegna)
072800161025     C                   EVAL      wTag = 'FTX+SIN'
072900161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073000161025     C                   EVAL      InStringa =
073100161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
073200161025     C                   EVAL      InSepara = '+'
073300161025     C                   EXSR      SR_SPLIT
073400161025     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
073500161025     C                                     %trim(SkSplitCSV_5(3))
073600161025     C                   ENDIF
073700161025     C*
073800161025     C***
073900161025     C* ...tipo record 'CNT+7' (peso spedizione)
074000161025     C                   EVAL      wTag = 'CNT+7'
074100161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074200161025     C                   EVAL      InStringa =
074300161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
074400161025     C                   EVAL      InSepara = ':'
074500161025     C                   EXSR      SR_SPLIT
074600161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
074700161025     C                   EXSR      CHKNUM
074800161025     C                   IF        PiNum=*on
074900161025     C                   Z-ADD     PiVal         VABPKB
075000161025     C                   ELSE
075100161025     C                   Z-ADD     1             wRecErr
075200161025     C                   ENDIF
075300161025     C                   ENDIF
075400161025     C*
075500161025     C***
075600161025     C* ...tipo record 'CNT+15' (volume spedizione)
075700161025     C                   EVAL      wTag = 'CNT+15'
075800161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075900161025     C                   EVAL      InStringa =
076000161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
076100161025     C                   EVAL      InSepara = ':'
076200161025     C                   EXSR      SR_SPLIT
076300161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
076400161025     C                   EXSR      CHKNUM
076500161025     C                   IF        PiNum=*on
076600161025     C                   Z-ADD     PiVal         VABVLB
076700161025     C                   ELSE
076800161025     C                   Z-ADD     1             wRecErr
076900161025     C                   ENDIF
077000161025     C                   ENDIF
077100161025     C*
077200161025     C***
077300161025     C* ...tipo record 'CNT+11' (numero colli)
077400161025     C                   EVAL      wTag = 'CNT+11'
077500161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077600161025     C                   EVAL      InStringa =
077700161025     C                                %subst(wvindta:%len(%trim(wTag))+2)
077800161025     C                   EVAL      InSepara = ':'
077900161025     C                   EXSR      SR_SPLIT
078000161025     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
078100161025     C                   EXSR      CHKNUM
078200161025     C                   IF        PiInt=*on
078300161025     C                   Z-ADD     PiVal         VABNCL
078400161025     C                   ELSE
078500161025     C                   Z-ADD     1             wRecErr
078600161025     C                   ENDIF
078700161025     C                   ENDIF
078800161025     C*
078900161025     C***
079000161025     C* ...tipo record 'RFF+CU' (riferimento spedizione)
079100161025     C                   EVAL      wTag = 'RFF+CU'
079200161025     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079300161025     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
079400161025     C                   EXSR      CHKNUM
079500161025     C                   IF        PiInt=*on
079600161025     C                   Z-ADD     PiVal         VABRMN
079700161025     C                   ELSE
079800161025     C                   EVAL      VABRMN = 1
079900161025     C                   Z-ADD     1             wRecErr
080000161025     C                   ENDIF
080100161025     C                   ENDIF
080200141028     C*
080300141028     C***
080400141028     C* ...tipo record 'NAD+CN' (destinatario)
080500141028     C                   EVAL      wTag = 'NAD+CN'
080600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080700141028     C                   EVAL      InStringa =
080800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
080900141028     C                   EVAL      InSepara = '+'
081000141028     C                   EXSR      SR_SPLIT
081100141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
081200141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
081300141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
081400141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
081500141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
081600141028     C                   IF        VABNZD = 'I'   OR
081700141028     C                             VABNZD = 'IT'  OR
081800141028     C                             VABNZD = 'ITA'
081900141028     C                   EVAL      VABNZD = *blanks
082000141028     C                   ENDIF
082100141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
082200141028     C                   EVAL      InSepara = ':'
082300141028     C                   EXSR      SR_SPLIT
082400141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
082500141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
082600141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
082700141028     C                                      %trim(SkSplitCSV_5(4))+
082800141028     C                                      %trim(SkSplitCSV_5(5))
082900141028     C                   ENDIF
083000150609     C*
083100150609     C***
083200150609     C* ...tipo record 'NAD+CZ' (mittente originale)
083300150609     C                   EVAL      wTag = 'NAD+CZ'
083400150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083500150609     C                   EVAL      InStringa =
083600150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
083700150609     C                   EVAL      InSepara = '+'
083800150609     C                   EXSR      SR_SPLIT
083900150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
084000150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
084100150609     C                   IF        VABNMO = 'I'   OR
084200150609     C                             VABNMO = 'IT'  OR
084300150609     C                             VABNMO = 'ITA'
084400150609     C                   EVAL      VABNMO = *blanks
084500161018     C                   ELSE
084600161018     C                   Z-ADD     1             jNAZ
084700161018     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
084800161018     C                   IF        %found
084900161018     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
085000161018     C                   ENDIF
085100161018     C                   ENDIF
085200161018     C*
085300150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
085400150609     C                   EVAL      InSepara = ':'
085500150609     C                   EXSR      SR_SPLIT
085600150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
085700150609     C                   ENDIF
085800161013     C*
085900161025     C***
086000161013     C* ...tipo record 'COM' (email destinatario ai fini alert)
086100161013     C                   EVAL      wTag = 'COM'
086200161013     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086300161013     C                   EVAL      InStringa =
086400161013     C                                %subst(wvindta:%len(%trim(wTag))+2)
086500161013     C                   EVAL      InSepara = ':'
086600161013     C                   EXSR      SR_SPLIT
086700161013     C                   IF        SkSplitCSV_5(2) = 'EM'
086800161013     C                   EVAL      wEMAIL = SkSplitCSV_5(1)
086900161013     C                   EVAL      VATNOT = %subst(wEMAIL:1:35)
087000161013     C                   EVAL      VATTRC = 'I'
087100161013     C                   EXSR      WRIVAT
087200161013     C                   EVAL      VATNOT = %subst(wEMAIL:36:35)
087300161013     C                   EVAL      VATTRC = 'J'
087400161013     C                   EXSR      WRIVAT
087500161013     C                   ENDIF
087600161013     C                   ENDIF
087700141028     C*
087800141028     C***
087900161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
088000161013     C                   EVAL      wTag = 'GIN+BJ'
088100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
088200150526     C                   EVAL      InStringa =
088300150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
088400161014     C     '+':':'       XLATE     InStringa     InStringa
088500161013     C                   EVAL      InSepara = ':'
088600150526     C                   EXSR      SR_SPLIT
088700150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
088800161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(1))
088900141028     C                   EVAL      VATTRC = 'E'
089000141028     C                   EXSR      WRIVAT
089100150526     C                   ENDIF
089200150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
089300161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(2))
089400150526     C                   EVAL      VATTRC = 'E'
089500150526     C                   EXSR      WRIVAT
089600150526     C                   ENDIF
089700150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
089800161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(3))
089900150526     C                   EVAL      VATTRC = 'E'
090000150526     C                   EXSR      WRIVAT
090100150526     C                   ENDIF
090200150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
090300161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(4))
090400150526     C                   EVAL      VATTRC = 'E'
090500150526     C                   EXSR      WRIVAT
090600150526     C                   ENDIF
090700150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
090800161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(5))
090900150526     C                   EVAL      VATTRC = 'E'
091000150526     C                   EXSR      WRIVAT
091100150526     C                   ENDIF
091200161017     C                   IF        %trim(SkSplitCSV_5(6)) <> *blanks
091300161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(6))
091400161017     C                   EVAL      VATTRC = 'E'
091500161017     C                   EXSR      WRIVAT
091600161017     C                   ENDIF
091700161017     C                   IF        %trim(SkSplitCSV_5(7)) <> *blanks
091800161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(7))
091900161017     C                   EVAL      VATTRC = 'E'
092000161017     C                   EXSR      WRIVAT
092100161017     C                   ENDIF
092200161017     C                   IF        %trim(SkSplitCSV_5(8)) <> *blanks
092300161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(8))
092400161017     C                   EVAL      VATTRC = 'E'
092500161017     C                   EXSR      WRIVAT
092600161017     C                   ENDIF
092700161017     C                   IF        %trim(SkSplitCSV_5(9)) <> *blanks
092800161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(9))
092900161017     C                   EVAL      VATTRC = 'E'
093000161017     C                   EXSR      WRIVAT
093100161017     C                   ENDIF
093200161017     C                   IF        %trim(SkSplitCSV_5(10)) <> *blanks
093300161025     C                   EVAL      VATNOT = '00'+%trim(SkSplitCSV_5(10))
093400161017     C                   EVAL      VATTRC = 'E'
093500161017     C                   EXSR      WRIVAT
093600161017     C                   ENDIF
093700141028     C                   ENDIF
093800141028     C*
093900141028     C***
094000141028     C* ...tipo record '???' (PDF per packing-list)
094100141028     C                   EVAL      wTag = '???PDF'
094200141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
094300141028     C*
094400141028     C* ......VATNOT_P
094500141028     C                   if        *in53
094600141028     C*
094700141028     C* Solo se valorizzato RMN
094800141028     C                   if        VABRMA <> *blanks
094900141028     C                   eval      pIn_MaskPDF =
095000141028     C                                     %trim(%subst(wvindta:1292:100))
095100141028     C                   call      'TIS7P2SER'
095200141028     C                   parm      'W'           pIn_Opz           1
095300141028     C                   parm                    tivlrds
095400141028     C                   parm                    EDIVABDS
095500141028     C                   parm      '10'          pIn_CdIdAz        2
095600141028     C                   parm                    pIn_MaskPDF      50
095700141028     C                   parm      *blanks       pOut_Esito        1
095800141028     C                   endif
095900141028     C                   endif
096000141028     C*
096100141028     C                   ENDIF
096200141028     C*
096300141028     C***
096400141028     C* ...tipo record 'UNT' (fine bolla)
096500170724     C                   EVAL      wTag = 'UNT'
096600170724     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
096700170724     C                   IF        *IN90
096800170724     C  N31              ADD       1             §CTROKVB
096900170724     C  N31              exsr      CHKWRI
097000170724     C  N31              WRITE     EDIVAB00
097100170724     C                   ENDIF
097200170724     C                   ENDIF
097300010202     C*
097400000801     C* Ebbene...
097500000801     C                   ADD       1             §CTRMO
097600141028     C                   IF        wRecErr <> *zeros
097700000801     C                   ADD       1             §CTRNO
097800000801     C                   EVAL      vinflg = '2'
097900000801     C                   ENDIF
098000000801     C*
098100000801     C                   ENDSR
098200010202     C*----------------------------------------------------*
098300140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
098400010202     C*----------------------------------------------------*
098500020305     C     PREVAT        BEGSR
098600010202     C*
098700140117     C* Compongo il nome del membro da dare al EDIVATWR
098800010202     C                   eval      parmbr = vlrhdl
098900010202     C                   movel     'M'           parmbr
099000060113     C                   eval      parccm = vlrksc
099100010202     C                   eval      paropz = '1'
099200010202     C* Effettuo la chiamata al CLLE preposto
099300140117     C                   call(e)   'TITVEVTC'
099400010202     C                   parm                    parccm
099500010202     C                   parm                    parmbr
099600010202     C                   parm                    paropz
099700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
099800010202     C                   if        %error
099900010202     C                   movel     '1'           chkcall
100000010202     C                   else
100100010202     C                   movel     '0'           chkcall
100200010202     C                   endif
100300010202     C*
100400010202     C                   ENDSR
100500141028     C*----------------------------------------------------*
100600141028     C*  CONTROLLO NUMERICITA' CAMPI
100700141028     C*----------------------------------------------------*
100800141028     C     CHKNUM        BEGSR
100900141028     C*
101000141028     C                   IF        PiDecChr = *blanks
101100141028     C                   EVAL      PiDecChr = CharNUM
101200141028     C                   ENDIF
101300141028     C*
101400141028     C                   callp     UBISNUM_Check(PiStr
101500141028     C                                          :PiDecChr
101600141028     C                                          :PiVal
101700141028     C                                          :PiNum
101800141028     C                                          :PiInt)
101900141028     C*
102000141028     C                   ENDSR
102100141028     C***
102200141028
102300141028
102400141028     C*----------------------------------------------------*
102500141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
102600141028     C*----------------------------------------------------*
102700141028     C     SR_SPLIT      BEGSR
102800141028     C*
102900141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
103000141028     C                   CALL      'XSPLIT2'
103100141028     C                   PARM                    InStringa
103200141028     C                   PARM                    InSepara
103300141028     C                   PARM      '5'           InTypeOut
103400141028     C                   PARM      ''            wSkParam
103500141028     C                   PARM                    OutErrore
103600141028     C                   PARM                    OutMsg
103700141028     C                   MOVEA     wSkParam      SkSplitCSV_5
103800141028     C*
103900141028     C                   ENDSR
104000141028
104100141028
104200141028     C*----------------------------------------------------*
104300141028     C*  SCRITTURA BUFFER EDIVAT
104400141028     C*----------------------------------------------------*
104500141028     C     WRIVAT        BEGSR
104600141028     C*
104700141028     C                   IF        VATNOT <> *blanks
104800141028     C                   EVAL      VATFGS = VABFGS
104900141028     C                   EVAL      VATCCM = VABCCM
105000141028     C                   EVAL      VATLNP = VABLNP
105100141028     C                   EVAL      VATAAS = VABAAS
105200141028     C                   EVAL      VATNRS = VABNRS
105300141028     C                   EVAL      VATNSP = VABNSP
105400141028     C                   EVAL      VATCMR = VABCMR
105500141028     C                   EVAL      VATCNT = VABCNT
105600141028     C                   ADD       1             §CTROKVT
105700141028     C                   WRITE     EDIVAT00
105800141028     C                   ENDIF
105900141028     C*
106000141028     C                   ENDSR
106100000801
106200011113
106300011113     C*----------------------------------------------------*
106400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
106500011113     C*----------------------------------------------------*
106600011113     C     CHKIMPDIV     BEGSR
106700011113     C*
106800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
106900011113     C                   Z-ADD     *zeros        wrkDec            9 9
107000011113     C*
107100011113     C* Come prima cosa effettuo considerazioni sulla divisa
107200011113     C                   IF        vabIAS > *zeros
107300011113     C                   IF        vabVAS <> 'EUR'
107400011113     C                   EVAL      vabVAS =  'ITL'
107500011113     C                   ENDIF
107600011113     C                   ENDIF
107700011113     C*
107800011113     C                   IF        vabCAS > *zeros
107900011113     C                   IF        vabVCA <> 'EUR'
108000011113     C                   EVAL      vabVCA =  'ITL'
108100011113     C                   ENDIF
108200011113     C                   ENDIF
108300011113     C*
108400011113     C                   IF        vabVMD > *zeros
108500020305     C                   IF        vabVAD <> 'EUR'
108600011113     C                   EVAL      vabVAD =  'ITL'
108700011113     C                   ENDIF
108800011113     C                   ENDIF
108900011113     C*
109000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
109100011113     C                   Z-ADD     vabIAS        wrkDec
109200011113     C                   IF        wrkDec > *zeros
109300011113     C                   IF        vabVAS = 'ITL'
109400011113     C                   EVAL      vabIAS = *zeros
109500011113     C                   ENDIF
109600011113     C                   ENDIF
109700011113     C*
109800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
109900011113     C                   Z-ADD     vabCAS        wrkDec
110000011113     C                   IF        wrkDec > *zeros
110100011113     C                   IF        vabVCA = 'ITL'
110200011113     C                   EVAL      vabCAS = *zeros
110300011113     C                   ENDIF
110400011113     C                   ENDIF
110500011113     C*
110600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
110700011113     C                   Z-ADD     vabVMD        wrkDec
110800011113     C                   IF        wrkDec > *zeros
110900011113     C                   IF        vabVAD = 'ITL'
111000011113     C                   EVAL      vabVMD = *zeros
111100011113     C                   ENDIF
111200011113     C                   ENDIF
111300011113     C*
111400011113     C                   ENDSR
111500011113     C***
111600011113
111700011113
111800000801
111900000801
112000990920      /TITLE Invio dei dati al punto operativo.
112100010202     C     invio         BEGSR
112200990920     C*
112300140117     C* 1° invio EDIVAT
112400010201     C                   reset                   dscmz
112500010201     C                   move      vlrpoi        cmzdst
112600140117     C                   eval      cmzfld = 'EDIVATWR'
112700010201     C                   eval      cmzmbd = vlrhdl
112800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
112900021009     C***                if        prmfir = *blanks
113000140117     C                   eval      cmzfla = 'EDIVAT0F'
113100140117     C                   eval      cmzmba = 'EDIVAT0F'
113200021009     C***                else
113300021009     C***                eval      cmzfla = prmfir
113400021009     C***                eval      cmzmba = prmfir
113500021009     C***                endif
113600010201     C                   eval      cmznrr = *zeros
113700020305     C                   move      §ctrokvt      cmznrr
113800021018     C                   eval      cmzlba = vlrfl1
113900010201     C                   call(e)   'TIS711C'
114000010201     C                   parm                    dscmz
114100010201     C                   parm      *blanks       esito
114200010205     C                   if        %error
114300010205     C                             or cmzerr = '1'
114400010205     C                             or esito  = '1'
114500010205     C                   eval      wrkesito = '3'
114600010205     C                   else
114700010201     C*
114800140117     C* 2° invio EDIVAB
114900010201     C                   reset                   dscmz
115000010201     C                   move      vlrpoi        cmzdst
115100010201     C                   eval      cmzfld = vlrfou
115200010201     C                   eval      cmzmbd = vlrhdl
115300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
115400021009     C***                if        prmfir = *blanks
115500140117     C                   eval      cmzfla = 'EDIVAB0F'
115600140117     C                   eval      cmzmba = 'EDIVAB0F'
115700021009     C***                else
115800021009     C***                eval      cmzfla = prmfir
115900021009     C***                eval      cmzmba = prmfir
116000021009     C***                endif
116100010201     C                   eval      cmznrr = *zeros
116200010201     C                   move      §ctrokvb      cmznrr
116300021018     C                   eval      cmzlba = vlrfl1
116400010201     C                   call(e)   'TIS711C'
116500010201     C                   parm                    dscmz
116600010201     C                   parm      *blanks       esito
116700010201     C                   if        %error
116800010201     C                             or cmzerr = '1'
116900010201     C                             or esito  = '1'
117000010201     C                   eval      wrkesito = '3'
117100010201     C                   endif
117200010205     C                   endif
117300990920     C*
117400000613     C                   ENDSR
117500000613     C***
117600070411
117700141028
117800070411     C     *pssr         BEGSR
117900070411     C*
118000070411     C                   if        %open(tivin00r)
118100070411     C                   close     tivin00r
118200070411     C                   endif
118300140117     C                   if        %open(edivabwr)
118400140117     C                   close     edivabwr
118500070411     C                   endif
118600140117     C                   if        %open(edivatwr)
118700140117     C                   close     edivatwr
118800070411     C                   endif
118900070411     C*
119000070411     C* Effettuo la chiamata al CLLE preposto
119100140117     C                   call(e)   'TITVEVTC'
119200070411     C                   parm                    parccm
119300070411     C                   parm                    parmbr
119400070411     C                   parm      '2'           paropz
119500070411     C*
119600070411     C                   eval      wrkesito = '2'
119700070411     C*
119800070411     C                   seton                                        LR
119900070411     C*
120000070411     C                   ENDSR     '*CANCL'
120100070411     C***
120200161018
120300161018
120400161018     C*--------------------------------------------------------
120500161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
120600161018     C*--------------------------------------------------------
120700161018     C     CARTAB        BEGSR
120800161018     C*
120900161018     C* TABELLA '15' - NAZIONI
121000161018     C                   clear                   skNAZISO
121100161018     C                   clear                   skNAZBAR
121200161018     C                   eval      tblKUT = 1
121300161018     C                   eval      tblCOD = '15'
121400161018     C     KEYtabP       setll     tabel00f
121500161018     C     KEYtabP       reade     tabel00f
121600161018     C                   dow       not %eof(tabel00f)
121700161018     C                   if        tblFLG = *blanks
121800161018     C                   movel(p)  tblUNI        ds15
121900161018     C                   add       1             jNAZ
122000161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
122100161018     C                   eval      skNAZISO(jNAZ) = §15COD
122200161018     C                   endif
122300161018     C     KEYtabP       reade     tabel00f
122400161018     C                   enddo
122500161018     C*
122600161018     C                   ENDSR
122700161018     C***
122800070411
122900990910
123000000613     C     *inzsr        BEGSR
123100990910     C*
123200990910     C     *entry        plist
123300990920     C                   parm                    tivlrds
123400990921     C                   parm      wrkesito      esito
123500000724     C                   parm                    prmlit
123600000710     C                   parm                    prmfir
123700000613     C*
123800000830     C* CALCOLA LA DATA CORRENTE
123900140117     C                   time                    wn14             14 0
124000140117     C                   movel     wn14          oracor            6 0          *ORA
124100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
124200100722     C                   eval      datcor = %dec(%date() : *ISO)
124300161018     C*
124400161018     C* Chiave su TABEL00F - parziale
124500161018     C     KEYtabP       KLIST
124600161018     C                   KFLD                    tblKUT
124700161018     C                   KFLD                    tblCOD
124800000830     C*
124900000613     C                   ENDSR
125000000613     C***
