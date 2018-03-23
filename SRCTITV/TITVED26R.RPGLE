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
009800000613     C*
009900161018     C                   EXSR      CARTAB                                       CARICA TABELLE
010000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100070719     C*
010200070719     C* Esegue lancio TISI95R solo x chiusura
010300070719     C                   CLEAR                   TISI95DS
010400070719     C                   EVAL      I95TLA = 'C'
010500070719     C                   CALL      'TISI95R'
010600070719     C                   PARM                    TISI95DS
010700000613     C*
010800010202     C* Effettuo la chiamata al CLLE preposto
010900140922     C                   call(e)   'TITVEVTC'
011000140922     C                   parm                    parccm
011100140922     C                   parm                    parmbr
011200140922     C                   parm      '2'           paropz
011300170731     C*
011400170731     C                   if        *in53
011500170731     C                   call      'TIS7P2SER'
011600170731     C                   parm      'C'           pIn_Opz           1
011700170731     C                   parm                    tivlrds
011800170731     C                   parm                    EDIVABDS
011900170731     C                   parm      *blanks       pIn_CdIdAz        2
012000170731     C                   parm      *blanks       pIn_MaskPDF      50
012100170731     C                   parm      *blanks       pOut_Esito        1
012200170731     C                   endif
012300000616     C*
012400010201     C                   seton                                        LR
012500990908
012600000801
012700910830     C*--------------------------------------------------------
012800140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
012900910830     C*--------------------------------------------------------
013000040526     C     RWFILE        BEGSR
013100990910     C*
013200990914     C                   if        not %open(tivin00r)
013300990908     C                   open      tivin00r
013400990914     C                   endif
013500140117     C                   if        not %open(edivabwr)
013600140117     C                   open      edivabwr
013700990914     C                   endif
013800141028     C*
013900140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014000020305     C                   exsr      prevat
014100010201     C*
014200010202     C                   if        chkcall = '0'
014300010202     C*
014400140117     C                   if        not %open(edivatwr)
014500140117     C                   open      edivatwr
014600010201     C                   endif
014700990910     C*
014800010201     C                   clear                   §CTROKVB          5 0
014900020305     C                   clear                   §CTROKVT          5 0
015000000801     C                   clear                   §CTRMO            5 0
015100000801     C                   clear                   §CTRNO            5 0
015200141028     C*
015300141028     C* Inizializzazioni fuori ciclo
015400141028     C                   EXSR      INZVAR
015500140117     C*
015600140902     C                   if        *in53
015700140117     C                   call      'TIS7P2SER'
015800140117     C                   parm      'O'           pIn_Opz           1
015900140117     C                   parm                    tivlrds
016000140117     C                   parm                    EDIVABDS
016100140117     C                   parm      *blanks       pIn_CdIdAz        2
016200140117     C                   parm      *blanks       pIn_MaskPDF      50
016300140117     C                   parm      *blanks       pOut_Esito        1
016400140902     C                   endif
016500130708     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900040910     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000801     C*
017200000801     C                   if        *in70 = *off
017300000801     C                             and
017400000801     C                             (vinflg = *blanks
017500000801     C                              or vinflg = '0'
017600000801     C                              or vinflg = '2')
017700000801     C*
017800000801     C                   clear                   vinmsg
017900141028     C*
018000141028     C* "normalizzo" il dato di input
018100141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
018200141028     C                                             %len(%trim(vindta))-1))
018300160209     C*
018400160209     C* Elimino le "duple di controllo EDIFACT"
018500160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
018600160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
018700160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
018800160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
018900160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
019000040910     C*
019100040910     C* Eseguo routine d traduzione
019200040910     C                   exsr      impvabvat
019300040802     C*
019400010305     C                   endif
019500000905     C*
019600000905     C                   else
019700000905     C                   eval      vinflg = '1'
019800000905     C                   endif
019900000905     C*
020000000905     C  N70              update    tivin000
020100000905     C*
020200991022     C  N70              ENDdo
020300100722     C*
020400100722     C* Scarico i buffer testata ancora "in canna"
020500140922     C*
020600141028     C***  N31              ADD       1             §CTROKVB
020700141028     C***  N31              exsr      CHKWRI
020800141028     C***  N31              WRITE     EDIVAB00
020900010202     C*
021000010202     C                   endif
021100990910
021200990910     C* Se non ci sono record con errori ...
021300000710     C                   if        §ctrno = 0
021400990910     C* ... restituisco esito OK.
021500990921     C                   eval      wrkesito = '0'
021600990910     C                   else
021700010201     C                   if        §ctrokvb > 0
021800990921     C                   eval      wrkesito = '1'
021900000710     C                   else
022000000710     C                   eval      wrkesito = '2'
022100990910     C                   endif
022200000710     C                   endif
022300990910     C*
022400990914     C                   if        %open(tivin00r)
022500990908     C                   close     tivin00r
022600990914     C                   endif
022700140117     C                   if        %open(edivabwr)
022800140117     C                   close     edivabwr
022900990914     C                   endif
023000140117     C                   if        %open(edivatwr)
023100140117     C                   close     edivatwr
023200010201     C                   endif
023300990910     C*
023400010201     C                   if        §ctrokvb > 0
023500000724     C                             and vlrpoi <> *zeros
023600010202     C                   exsr      invio
023700990920     C                   endif
023800990920     C*
023900910830     C                   ENDSR
024000000613     C***
024100140922
024200140922     C*----------------------------------------------------*
024300140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
024400140922     C*----------------------------------------------------*
024500140922     C     CHKWRI        BEGSR
024600140922     C*
024700141028     C                   if        VABRMN = *zeros
024800141028     C                   eval      VABRMN = VABNSP
024900141028     C                   endif
025000170214     C*
025100170214     C* Se peso non indicato nei dati cleinte e richiesto un peso forzato per collo
025200170214     C                   IF        VABPKB = *zeros
025300170214     C                   EVAL      VABPKB = wVABPKN * VABNCL
025400170214     C                   ENDIF
025500141028     C*
025600141028     C                   if        VABPRD = *blanks
025700141028     C* ......VABPRD
025800141028     C* Reperisco la provincia dal CAP e dalla località
025900141028     C                   IF        VABLOD <> *blanks AND
026000141028     C                             VABCAD <> *blanks AND
026100141028     C                             VABNZD  = *blanks
026200141028     C                   CLEAR                   TISI95DS
026300141028     C                   EVAL      I95TCN = '3'
026400141028     C                   Z-ADD     datcor        I95DAT
026500141028     C                   EVAL      I95CAP = VABCAD
026600141028     C                   EVAL      I95LOC = VABLOD
026700141028     C                   CALL      'TISI95R'
026800141028     C                   PARM                    TISI95DS
026900141028     C                   EVAL      VABPRD = O95PRV
027000141028     C                   ENDIF
027100141028     C                   endif
027200140922     C*
027300140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
027400141028     C                   IF        wFlgCAS <> '0'
027500140922     C                   IF        VABCBO = '1'
027600140922     C                   EVAL      VABCBO = '4'
027700140922     C                   ELSE
027800140922     C                   EVAL      VABCBO = '6'
027900140922     C                   ENDIF
028000140922     C                   ENDIF
028100140922     C*
028200140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
028300141028     C                   EXSR      CHKIMPDIV
028400150527     C*
028500150527     C* Scompongo le note
028600150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
028700150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
028800140922     C*
028900140922     C                   ENDSR
029000141028     C*----------------------------------------------------*
029100141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029200141028     C*----------------------------------------------------*
029300141028     C     INZVAR        BEGSR
029400141028     C*
029500141028     C* Inizializzo variabili di wrk
029600141028     C                   Z-ADD     *zeros        Num5_0            5 0
029700141028     C                   MOVEL     '0'           wFlgCAS
029800141028     C                   MOVEL     *blanks       wVATNOT_A
029900141028     C                   MOVEL     *blanks       wVATNOT_B
030000141028     C                   MOVEL     *blanks       wVATNOT_E
030100141028     C                   MOVEL     *blanks       wVATNOT_I
030200141028     C                   MOVEL     *blanks       wVATNOT_J
030300141028     C                   MOVEL     *blanks       wVATNOT_S
030400141028     C                   MOVEL     *blanks       wVATNOT_P
030500141028     C                   CLEAR                   SkSplitCSV_5
030600150527     C                   MOVEL     *blanks       wNOTE
030700141028     C*
030800141028     C* Reimposto i valori di default
030900141028     C                   EXSR      DEFCAM
031000141028     C*
031100141028     C                   ENDSR
031200141028     C*----------------------------------------------------*
031300141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
031400141028     C*----------------------------------------------------*
031500141028     C     DEFCAM        BEGSR
031600141028     C*
031700141028     C                   CLEAR                   EDIVAB00
031800141028     C                   CLEAR                   EDIVAT00
031900141028     C*
032000141028     C* Imposto i valori bolla di default
032100141028     C                   EVAL      VABCTR = 000
032200141028     C                   EVAL      VABCBO = '1'
032300141104     C                   EVAL      VABTSP = 'C'
032400141028     C*
032500141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032600141028     C* e delimitatore testo.
032700141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032800141028     C*
032900141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
033000141028     C                   EVAL      CharSOS = CharNUM
033100141028     C*
033200141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
033300141028     C                   SETOFF                                       53
033400141028     C                   SELECT
033500141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
033600141028     C                   SETON                                        53
033700141028     C                   ENDSL
033800141028     C*
033900141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
034000141028     C                   EVAL      posDaDft = 1
034100141028     C                   EVAL      posADft  = 0
034200141028     C                   EVAL      wGiroDft = 0
034300141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034400141028     C                             posDaDft > 0
034500141028     C*
034600141028     C* Gestisco il 1° giro
034700141028     C                   IF        wGiroDft = 0
034800141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
034900141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
035000141028     C* Incremento il contatore dei "giri"
035100141028     C                   EVAL      wGiroDft = 1
035200141028     C                   ELSE
035300141028     C                   EVAL      posDaDft = posADft
035400141028     C                   ENDIF
035500141028     C* Eseguo lo scan x trovare la fine del campo corrente
035600141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035700141028     C*
035800141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
035900141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
036000141028     C                   IF        posDaDft > 0 AND
036100141028     C                             posADft  > 0
036200141028     C* NCL
036300141028     C                   IF        %subst(
036400141028     C                             %subst(vlrppt:posDaDft+1:
036500141028     C                             posADft-posDaDft-1):1:3)
036600141028     C                             = 'NCL'
036700141028     C                   EVAL      PiStr=%trim(%subst(
036800141028     C                             %subst(vlrppt:posDaDft+1:
036900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037000141028     C                   EXSR      CHKNUM
037100141028     C                   IF        PiInt=*on
037200141028     C                   Z-ADD     PiVal         VABNCL
037300141028     C                   ENDIF
037400141028     C                   ENDIF
037500141028     C* CCM
037600141028     C                   IF        %subst(
037700141028     C                             %subst(vlrppt:posDaDft+1:
037800141028     C                             posADft-posDaDft-1):1:3)
037900141028     C                             = 'CCM'
038000141028     C                   EVAL      PiStr=%trim(%subst(
038100141028     C                             %subst(vlrppt:posDaDft+1:
038200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038300141028     C                   EXSR      CHKNUM
038400141028     C                   IF        PiInt=*on
038500141028     C                   Z-ADD     PiVal         VABCCM
038600141028     C                   ENDIF
038700141028     C                   ENDIF
038800141028     C* LNP
038900141028     C                   IF        %subst(
039000141028     C                             %subst(vlrppt:posDaDft+1:
039100141028     C                             posADft-posDaDft-1):1:3)
039200141028     C                             = 'LNP'
039300141028     C                   EVAL      PiStr=%trim(%subst(
039400141028     C                             %subst(vlrppt:posDaDft+1:
039500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039600141028     C                   EXSR      CHKNUM
039700141028     C                   IF        PiInt=*on
039800141028     C                   Z-ADD     PiVal         VABLNP
039900141028     C                   ENDIF
040000141028     C                   ENDIF
040100141028     C* NRS
040200141028     C                   IF        %subst(
040300141028     C                             %subst(vlrppt:posDaDft+1:
040400141028     C                             posADft-posDaDft-1):1:3)
040500141028     C                             = 'NRS'
040600141028     C                   EVAL      PiStr=%trim(%subst(
040700141028     C                             %subst(vlrppt:posDaDft+1:
040800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040900141028     C                   EXSR      CHKNUM
041000141028     C                   IF        PiInt=*on
041100141028     C                   Z-ADD     PiVal         VABNRS
041200141028     C                   ENDIF
041300141028     C                   ENDIF
041400141028     C* CTR
041500141028     C                   IF        %subst(
041600141028     C                             %subst(vlrppt:posDaDft+1:
041700141028     C                             posADft-posDaDft-1):1:3)
041800141028     C                             = 'CTR'
041900141028     C                   EVAL      PiStr=%trim(%subst(
042000141028     C                             %subst(vlrppt:posDaDft+1:
042100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200141028     C                   EXSR      CHKNUM
042300141028     C                   IF        PiInt=*on
042400141028     C                   Z-ADD     PiVal         VABCTR
042500141028     C                   ENDIF
042600141028     C                   ENDIF
042700141028     C* PKB
042800141028     C                   IF        %subst(
042900141028     C                             %subst(vlrppt:posDaDft+1:
043000141028     C                             posADft-posDaDft-1):1:3)
043100141028     C                             = 'PKB'
043200141028     C                   EVAL      PiStr=%trim(%subst(
043300141028     C                             %subst(vlrppt:posDaDft+1:
043400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500141028     C                   EXSR      CHKNUM
043600141028     C                   IF        PiNum=*on
043700141028     C                   Z-ADD     PiVal         VABPKB
043800141028     C                   ENDIF
043900141028     C                   ENDIF
044000141028     C* VLB
044100141028     C                   IF        %subst(
044200141028     C                             %subst(vlrppt:posDaDft+1:
044300141028     C                             posADft-posDaDft-1):1:3)
044400141028     C                             = 'VLB'
044500141028     C                   EVAL      PiStr=%trim(%subst(
044600141028     C                             %subst(vlrppt:posDaDft+1:
044700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044800141028     C                   EXSR      CHKNUM
044900141028     C                   IF        PiNum=*on
045000141028     C                   Z-ADD     PiVal         VABVLB
045100141028     C                   ENDIF
045200141028     C                   ENDIF
045300141028     C* QFT
045400141028     C                   IF        %subst(
045500141028     C                             %subst(vlrppt:posDaDft+1:
045600141028     C                             posADft-posDaDft-1):1:3)
045700141028     C                             = 'QFT'
045800141028     C                   EVAL      PiStr=%trim(%subst(
045900141028     C                             %subst(vlrppt:posDaDft+1:
046000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046100141028     C                   EXSR      CHKNUM
046200141028     C                   IF        PiNum=*on
046300141028     C                   Z-ADD     PiVal         VABQFT
046400141028     C                   ENDIF
046500141028     C                   ENDIF
046600141028     C* CBO
046700141028     C                   IF        %subst(
046800141028     C                             %subst(vlrppt:posDaDft+1:
046900141028     C                             posADft-posDaDft-1):1:3)
047000141028     C                             = 'CBO'
047100141028     C                   EVAL      VABCBO=%trim(%subst(
047200141028     C                             %subst(vlrppt:posDaDft+1:
047300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047400141028     C                   ENDIF
047500141028     C* TSP
047600141028     C                   IF        %subst(
047700141028     C                             %subst(vlrppt:posDaDft+1:
047800141028     C                             posADft-posDaDft-1):1:3)
047900141028     C                             = 'TSP'
048000141028     C                   EVAL      VABTSP=%trim(%subst(
048100141028     C                             %subst(vlrppt:posDaDft+1:
048200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048300141028     C                   ENDIF
048400141028     C* VAS
048500141028     C                   IF        %subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):1:3)
048800141028     C                             = 'VAS'
048900141028     C                   EVAL      VABVAS=%trim(%subst(
049000141028     C                             %subst(vlrppt:posDaDft+1:
049100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049200141028     C                   ENDIF
049300141028     C* VCA
049400141028     C                   IF        %subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):1:3)
049700141028     C                             = 'VCA'
049800141028     C                   EVAL      VABVCA=%trim(%subst(
049900141028     C                             %subst(vlrppt:posDaDft+1:
050000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100141028     C                   ENDIF
050200141028     C* TIC
050300141028     C                   IF        %subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):1:3)
050600141028     C                             = 'TIC'
050700141028     C                   EVAL      VABTIC=%trim(%subst(
050800141028     C                             %subst(vlrppt:posDaDft+1:
050900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000141028     C                   ENDIF
051100141028     C* GCA
051200141028     C                   IF        %subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):1:3)
051500141028     C                             = 'GCA'
051600141028     C                   EVAL      VABGCA=%trim(%subst(
051700141028     C                             %subst(vlrppt:posDaDft+1:
051800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051900141028     C                   ENDIF
052000141028     C* CTM
052100141028     C                   IF        %subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):1:3)
052400141028     C                             = 'CTM'
052500141028     C                   EVAL      VABCTM=%trim(%subst(
052600141028     C                             %subst(vlrppt:posDaDft+1:
052700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052800141028     C                   ENDIF
052900141028     C* FFD
053000141028     C                   IF        %subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):1:3)
053300141028     C                             = 'FFD'
053400141028     C                   EVAL      VABFFD=%trim(%subst(
053500141028     C                             %subst(vlrppt:posDaDft+1:
053600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700141028     C                   ENDIF
053800141028     C* VAD
053900141028     C                   IF        %subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):1:3)
054200141028     C                             = 'VAD'
054300141028     C                   EVAL      VABVAD=%trim(%subst(
054400141028     C                             %subst(vlrppt:posDaDft+1:
054500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600141028     C                   ENDIF
054700141028     C* GMA
054800141028     C                   IF        %subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000141028     C                             posADft-posDaDft-1):1:3)
055100141028     C                             = 'GMA'
055200141028     C                   EVAL      VABGMA=%trim(%subst(
055300141028     C                             %subst(vlrppt:posDaDft+1:
055400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500141028     C                   ENDIF
055600141028     C* GGA
055700141028     C                   IF        %subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):1:3)
056000141028     C                             = 'GGA'
056100141028     C                   EVAL      VABGGA=%trim(%subst(
056200141028     C                             %subst(vlrppt:posDaDft+1:
056300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400141028     C                   ENDIF
056500141028     C* GVA
056600141028     C                   IF        %subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):1:3)
056900141028     C                             = 'GVA'
057000141028     C                   EVAL      VABGVA=%trim(%subst(
057100141028     C                             %subst(vlrppt:posDaDft+1:
057200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300141028     C                   ENDIF
057400141028     C* TC1
057500141028     C                   IF        %subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):1:3)
057800141028     C                             = 'TC1'
057900141028     C                   EVAL      VABTC1=%trim(%subst(
058000141028     C                             %subst(vlrppt:posDaDft+1:
058100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200141028     C                   ENDIF
058300141028     C* TC2
058400141028     C                   IF        %subst(
058500141028     C                             %subst(vlrppt:posDaDft+1:
058600141028     C                             posADft-posDaDft-1):1:3)
058700141028     C                             = 'TC2'
058800141028     C                   EVAL      VABTC2=%trim(%subst(
058900141028     C                             %subst(vlrppt:posDaDft+1:
059000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100141028     C                   ENDIF
059200141028     C* VATTRC
059300141028     C                   IF        %subst(
059400141028     C                             %subst(vlrppt:posDaDft+1:
059500141028     C                             posADft-posDaDft-1):1:3)
059600141028     C                             = 'TRC'
059700141028     C                   EVAL      VATTRC=%trim(%subst(
059800141028     C                             %subst(vlrppt:posDaDft+1:
059900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000141028     C                   ENDIF
060100170214     C*
060200170214     C***
060300170214     C* Campi "personalizzati"
060400170214     C***
060500170214     C* PKN
060600170214     C                   IF        %subst(
060700170214     C                             %subst(vlrppt:posDaDft+1:
060800170214     C                             posADft-posDaDft-1):1:3)
060900170214     C                             = 'PKN'
061000170214     C                   EVAL      PiStr=%trim(%subst(
061100170214     C                             %subst(vlrppt:posDaDft+1:
061200170214     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300170214     C                   EXSR      CHKNUM
061400170214     C                   IF        PiNum=*on
061500170214     C                   Z-ADD     PiVal         wVABPKN           7 1
061600170214     C                   ENDIF
061700170214     C                   ENDIF
061800170214     C***
061900141028     C* ...
062000141028     C                   ENDIF
062100141028     C                   ENDDO
062200141028     C*
062300141028     C                   ENDSR
062400000801     C*----------------------------------------------------*
062500140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
062600000801     C*----------------------------------------------------*
062700040910     C     IMPVABVAT     BEGSR
062800040910     C*
062900040910     C* Traduzione relativa ai tipi record del file del cliente
063000040910     C*
063100071210     C*
063200071210     C***
063300141028     C* ...tipo record 'UNB' (info CMR)
063400141028     C                   EVAL      wTag = 'UNB'
063500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
063600141028     C                   MOVEL     *blanks       savCMR           35
063700141028     C                   EVAL      InStringa =
063800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
063900141028     C                   EVAL      InSepara = '+'
064000141028     C                   EXSR      SR_SPLIT
064100141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
064200140922     C                                      %char(datcor) + ' ' + %char(oracor)
064300130620     C                   ENDIF
064400141028     C*
064500141028     C***
064600161025     C* ...tipo record 'UNH' (rottuta di codice spedizione)
064700161025     C                   EVAL      wTag = 'UNH'
064800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
064900150526     C*
065000150526     C* Inizializzazioni per nuova spedizione
065100141028     C                   SETOFF                                       31
065200141028     C                   eval      vinflg = '1'
065300141028     C* ......inizializzazioni iniziali e formati record file Bartolini
065400141028     C                   EXSR      INZVAR
065500141028     C* ......valorizzazione campi fissi
065600141028     C                   MOVEL     datcor        VABAAS
065700141028     C                   MOVE      datcor        VABMGS
065800141028     C                   MOVE(P)   vlrpoi        VABFGS
065900141028     C                   EVAL      VABCMR = savCMR
066000141028     C                   EVAL      VABDCM = datcor
066100141028     C                   EVAL      VABDTS = datcor
066200141028     C                   EVAL      VABCNT = 1
066300141028     C* ......NSP => Stacco un numeratore da AZNUM
066400141028     C                   clear                   TRUL33DS
066500141028     C                   eval      I33OPE = *zeros
066600141028     C                   eval      I33CNU = 302
066700141028     C                   eval      I33NUM = 1
066800141028     C                   movel     TRUL33DS      KPJBU
066900141028     C                   call      'TRUL33R'
067000141028     C                   parm                    KPJBA
067100141028     C                   movel     KPJBU         TRUL33DS
067200141028     C                   if        O33ERR = *zeros
067300141028     C                   z-add     O33NRF        VABNSP
067400141028     C                   else
067500141028     C                   Z-ADD     1             wRecErr
067600141028     C                   SETON                                        31
067700141028     C                   EVAL      vinmsg = %trimr(vinmsg)
067800141028     C                             + ' ' + 'VABNSP VATNSP'
067900141028     C                   endif
068000141028     C                   ENDIF
068100161019     C*
068200161019     C***
068300170208     C* ...tipo record 'BGM' (riferimento spedizione)
068400170208     C                   EVAL      wTag = 'BGM+610'
068500161019     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068600161019     C                   EVAL      InStringa =
068700161019     C                                %subst(wvindta:%len(%trim(wTag))+2)
068800161025     C                   EVAL      InSepara = '+'
068900161019     C                   EXSR      SR_SPLIT
069000170208     C                   EVAL      VABRMA = %trim(SkSplitCSV_5(1))
069100161019     C                   ENDIF
069200150527     C*
069300150527     C***
069400161025     C* ...tipo record 'DTM+63' (data consegna richiesta)
069500161025     C                   EVAL      wTag = 'DTM+63'
069600150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069700150527     C                   EVAL      PiStr=%subst(
069800150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
069900150527     C                   EXSR      CHKNUM
070000150527     C                   IF        PiInt=*on
070100150527     C                   Z-ADD     PiVal         VABDCR
070200150527     C                   ELSE
070300150527     C                   Z-ADD     1             wRecErr
070400150527     C                   ENDIF
070500150527     C                   ENDIF
070600161025     C*
070700161025     C***
070800161025     C* ...tipo record 'DTM+64' (data consegna richiesta)
070900170208     C***                EVAL      wTag = 'DTM+64'
071000170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071100170208     C***                EVAL      PiStr=%subst(
071200170208     C***                            %subst(wvindta:%len(%trim(wTag))+2):1:8)
071300170208     C***                EXSR      CHKNUM
071400170208     C***                IF        PiInt=*on
071500170208     C***                Z-ADD     PiVal         VABDCR
071600170208     C***                ELSE
071700170208     C***                Z-ADD     1             wRecErr
071800170208     C***                ENDIF
071900170208     C***                ENDIF
072000161025     C*
072100161025     C***
072200161025     C* ...tipo record 'MOA+22' (contrassegno / divisa)
072300170208     C***                EVAL      wTag = 'MOA+22'
072400170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
072500170208     C***                EVAL      wFlgCAS = '1'
072600170208     C***                EVAL      InStringa =
072700170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
072800170208     C***                EVAL      InSepara = ':'
072900170208     C***                EXSR      SR_SPLIT
073000170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
073100170208     C***                EXSR      CHKNUM
073200170208     C***                IF        PiNum=*on
073300170208     C***                ADD       PiVal         VABCAS
073400170208     C***                EVAL      VABVCA = %trim(SkSplitCSV_5(2))
073500170208     C***                ELSE
073600170208     C***                Z-ADD     1             wRecErr
073700170208     C***                ENDIF
073800170208     C***                ENDIF
073900161025     C*
074000161025     C***
074100161025     C* ...tipo record 'FTX+SIN' (note di consegna)
074200170208     C***                EVAL      wTag = 'FTX+SIN'
074300170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074400170208     C***                EVAL      InStringa =
074500170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
074600170208     C***                EVAL      InSepara = '+'
074700170208     C***                EXSR      SR_SPLIT
074800170208     C***                EVAL      wNOTE = %trim(wNOTE) + ' ' +
074900170208     C***                                  %trim(SkSplitCSV_5(3))
075000170208     C***                ENDIF
075100161025     C*
075200161025     C***
075300161025     C* ...tipo record 'CNT+7' (peso spedizione)
075400170208     C***                EVAL      wTag = 'CNT+7'
075500170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
075600170208     C***                EVAL      InStringa =
075700170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
075800170208     C***                EVAL      InSepara = ':'
075900170208     C***                EXSR      SR_SPLIT
076000170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
076100170208     C***                EXSR      CHKNUM
076200170208     C***                IF        PiNum=*on
076300170208     C***                Z-ADD     PiVal         VABPKB
076400170208     C***                ELSE
076500170208     C***                Z-ADD     1             wRecErr
076600170208     C***                ENDIF
076700170208     C***                ENDIF
076800161025     C*
076900161025     C***
077000161025     C* ...tipo record 'CNT+15' (volume spedizione)
077100170208     C***                EVAL      wTag = 'CNT+15'
077200170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077300170208     C***                EVAL      InStringa =
077400170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
077500170208     C***                EVAL      InSepara = ':'
077600170208     C***                EXSR      SR_SPLIT
077700170208     C***                EVAL      PiStr=%trim(SkSplitCSV_5(1))
077800170208     C***                EXSR      CHKNUM
077900170208     C***                IF        PiNum=*on
078000170208     C***                Z-ADD     PiVal         VABVLB
078100170208     C***                ELSE
078200170208     C***                Z-ADD     1             wRecErr
078300170208     C***                ENDIF
078400170208     C***                ENDIF
078500161025     C*
078600161025     C***
078700170209     C* ...tipo record 'GID+1' (numero colli)
078800170209     C                   EVAL      wTag = 'GID+1'
078900170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079000170209     C                   EVAL      InStringa =
079100170209     C                                %subst(wvindta:%len(%trim(wTag))+2)
079200170209     C                   EVAL      InSepara = ':'
079300170209     C                   EXSR      SR_SPLIT
079400170209     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
079500170209     C                   EXSR      CHKNUM
079600170209     C                   IF        PiInt=*on
079700170209     C                   Z-ADD     PiVal         VABNCL
079800170209     C                   ELSE
079900170209     C                   Z-ADD     1             wRecErr
080000170209     C                   ENDIF
080100170209     C                   ENDIF
080200170209     C*
080300170209     C***
080400170209     C* ...tipo record 'RFF+CR' (riferimento spedizione)
080500170209     C                   EVAL      wTag = 'RFF+CR'
080600170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080700170209     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
080800170209     C                   EXSR      CHKNUM
080900170209     C                   IF        PiInt=*on
081000170209     C                   Z-ADD     PiVal         VABRMN
081100170209     C                   ELSE
081200170209     C                   EVAL      VABRMN = 1
081300170209     C                   Z-ADD     1             wRecErr
081400170209     C                   ENDIF
081500170209     C                   ENDIF
081600170209     C*
081700170209     C***
081800170209     C* ...tipo record 'RFF+CU' (riferimento spedizione)
081900170209     C                   EVAL      wTag = 'RFF+CU'
082000170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
082100170209     C                   EVAL      PiStr=%subst(wvindta:%len(%trim(wTag))+2)
082200170209     C                   EXSR      CHKNUM
082300170209     C                   IF        PiInt=*on
082400170209     C                   Z-ADD     PiVal         VABRMN
082500170209     C                   ELSE
082600170209     C                   EVAL      VABRMN = 1
082700170209     C                   Z-ADD     1             wRecErr
082800170209     C                   ENDIF
082900170209     C                   ENDIF
083000141028     C*
083100141028     C***
083200170208     C* ...tipo record 'NAD+DP' (destinatario)
083300170208     C                   EVAL      wTag = 'NAD+DP'
083400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083500141028     C                   EVAL      InStringa =
083600141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
083700141028     C                   EVAL      InSepara = '+'
083800141028     C                   EXSR      SR_SPLIT
083900141028     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
084000141028     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
084100141028     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
084200141028     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
084300141028     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
084400141028     C                   IF        VABNZD = 'I'   OR
084500141028     C                             VABNZD = 'IT'  OR
084600141028     C                             VABNZD = 'ITA'
084700141028     C                   EVAL      VABNZD = *blanks
084800141028     C                   ENDIF
084900141028     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
085000141028     C                   EVAL      InSepara = ':'
085100141028     C                   EXSR      SR_SPLIT
085200141028     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
085300141028     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
085400141028     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
085500141028     C                                      %trim(SkSplitCSV_5(4))+
085600141028     C                                      %trim(SkSplitCSV_5(5))
085700141028     C                   ENDIF
085800150609     C*
085900150609     C***
086000150609     C* ...tipo record 'NAD+CZ' (mittente originale)
086100150609     C                   EVAL      wTag = 'NAD+CZ'
086200150609     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
086300150609     C                   EVAL      InStringa =
086400150609     C                                %subst(wvindta:%len(%trim(wTag))+2)
086500150609     C                   EVAL      InSepara = '+'
086600150609     C                   EXSR      SR_SPLIT
086700150609     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
086800150609     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
086900150609     C                   IF        VABNMO = 'I'   OR
087000150609     C                             VABNMO = 'IT'  OR
087100150609     C                             VABNMO = 'ITA'
087200150609     C                   EVAL      VABNMO = *blanks
087300161018     C                   ELSE
087400161018     C                   Z-ADD     1             jNAZ
087500161018     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
087600161018     C                   IF        %found
087700161018     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
087800161018     C                   ENDIF
087900161018     C                   ENDIF
088000161018     C*
088100150609     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
088200150609     C                   EVAL      InSepara = ':'
088300150609     C                   EXSR      SR_SPLIT
088400150609     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
088500150609     C                   ENDIF
088600161013     C*
088700161025     C***
088800161013     C* ...tipo record 'COM' (email destinatario ai fini alert)
088900170208     C***                EVAL      wTag = 'COM'
089000170208     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
089100170208     C***                EVAL      InStringa =
089200170208     C***                             %subst(wvindta:%len(%trim(wTag))+2)
089300170208     C***                EVAL      InSepara = ':'
089400170208     C***                EXSR      SR_SPLIT
089500170208     C***                IF        SkSplitCSV_5(2) = 'EM'
089600170208     C***                EVAL      wEMAIL = SkSplitCSV_5(1)
089700170208     C***                EVAL      VATNOT = %subst(wEMAIL:1:35)
089800170208     C***                EVAL      VATTRC = 'I'
089900170208     C***                EXSR      WRIVAT
090000170208     C***                EVAL      VATNOT = %subst(wEMAIL:36:35)
090100170208     C***                EVAL      VATTRC = 'J'
090200170208     C***                EXSR      WRIVAT
090300170208     C***                ENDIF
090400170208     C***                ENDIF
090500141028     C*
090600141028     C***
090700161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
090800161013     C                   EVAL      wTag = 'GIN+BJ'
090900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
091000150526     C                   EVAL      InStringa =
091100150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
091200161014     C     '+':':'       XLATE     InStringa     InStringa
091300161013     C                   EVAL      InSepara = ':'
091400150526     C                   EXSR      SR_SPLIT
091500150526     C                   IF        %trim(SkSplitCSV_5(1)) <> *blanks
091600170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(1))
091700141028     C                   EVAL      VATTRC = 'E'
091800141028     C                   EXSR      WRIVAT
091900150526     C                   ENDIF
092000150526     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
092100170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(2))
092200150526     C                   EVAL      VATTRC = 'E'
092300150526     C                   EXSR      WRIVAT
092400150526     C                   ENDIF
092500150526     C                   IF        %trim(SkSplitCSV_5(3)) <> *blanks
092600170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(3))
092700150526     C                   EVAL      VATTRC = 'E'
092800150526     C                   EXSR      WRIVAT
092900150526     C                   ENDIF
093000150526     C                   IF        %trim(SkSplitCSV_5(4)) <> *blanks
093100170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(4))
093200150526     C                   EVAL      VATTRC = 'E'
093300150526     C                   EXSR      WRIVAT
093400150526     C                   ENDIF
093500150526     C                   IF        %trim(SkSplitCSV_5(5)) <> *blanks
093600170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(5))
093700150526     C                   EVAL      VATTRC = 'E'
093800150526     C                   EXSR      WRIVAT
093900150526     C                   ENDIF
094000161017     C                   IF        %trim(SkSplitCSV_5(6)) <> *blanks
094100170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(6))
094200161017     C                   EVAL      VATTRC = 'E'
094300161017     C                   EXSR      WRIVAT
094400161017     C                   ENDIF
094500161017     C                   IF        %trim(SkSplitCSV_5(7)) <> *blanks
094600170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(7))
094700161017     C                   EVAL      VATTRC = 'E'
094800161017     C                   EXSR      WRIVAT
094900161017     C                   ENDIF
095000161017     C                   IF        %trim(SkSplitCSV_5(8)) <> *blanks
095100170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(8))
095200161017     C                   EVAL      VATTRC = 'E'
095300161017     C                   EXSR      WRIVAT
095400161017     C                   ENDIF
095500161017     C                   IF        %trim(SkSplitCSV_5(9)) <> *blanks
095600170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(9))
095700161017     C                   EVAL      VATTRC = 'E'
095800161017     C                   EXSR      WRIVAT
095900161017     C                   ENDIF
096000161017     C                   IF        %trim(SkSplitCSV_5(10)) <> *blanks
096100170208     C                   EVAL      VATNOT = %trim(SkSplitCSV_5(10))
096200161017     C                   EVAL      VATTRC = 'E'
096300161017     C                   EXSR      WRIVAT
096400161017     C                   ENDIF
096500141028     C                   ENDIF
096600141028     C*
096700141028     C***
096800141028     C* ...tipo record '???' (PDF per packing-list)
096900141028     C                   EVAL      wTag = '???PDF'
097000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097100141028     C*
097200141028     C* ......VATNOT_P
097300141028     C                   if        *in53
097400141028     C*
097500141028     C* Solo se valorizzato RMN
097600141028     C                   if        VABRMA <> *blanks
097700141028     C                   eval      pIn_MaskPDF =
097800141028     C                                     %trim(%subst(wvindta:1292:100))
097900141028     C                   call      'TIS7P2SER'
098000141028     C                   parm      'W'           pIn_Opz           1
098100141028     C                   parm                    tivlrds
098200141028     C                   parm                    EDIVABDS
098300141028     C                   parm      '10'          pIn_CdIdAz        2
098400141028     C                   parm                    pIn_MaskPDF      50
098500141028     C                   parm      *blanks       pOut_Esito        1
098600141028     C                   endif
098700141028     C                   endif
098800141028     C*
098900141028     C                   ENDIF
099000141028     C*
099100141028     C***
099200141028     C* ...tipo record 'UNT' (fine bolla)
099300141028     C                   EVAL      wTag = 'UNT'
099400141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
099500141028     C  N31              ADD       1             §CTROKVB
099600141028     C  N31              exsr      CHKWRI
099700140117     C  N31              WRITE     EDIVAB00
099800150526     C                   ENDIF
099900010202     C*
100000000801     C* Ebbene...
100100000801     C                   ADD       1             §CTRMO
100200141028     C                   IF        wRecErr <> *zeros
100300000801     C                   ADD       1             §CTRNO
100400000801     C                   EVAL      vinflg = '2'
100500000801     C                   ENDIF
100600000801     C*
100700000801     C                   ENDSR
100800010202     C*----------------------------------------------------*
100900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
101000010202     C*----------------------------------------------------*
101100020305     C     PREVAT        BEGSR
101200010202     C*
101300140117     C* Compongo il nome del membro da dare al EDIVATWR
101400010202     C                   eval      parmbr = vlrhdl
101500010202     C                   movel     'M'           parmbr
101600060113     C                   eval      parccm = vlrksc
101700010202     C                   eval      paropz = '1'
101800010202     C* Effettuo la chiamata al CLLE preposto
101900140117     C                   call(e)   'TITVEVTC'
102000010202     C                   parm                    parccm
102100010202     C                   parm                    parmbr
102200010202     C                   parm                    paropz
102300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
102400010202     C                   if        %error
102500010202     C                   movel     '1'           chkcall
102600010202     C                   else
102700010202     C                   movel     '0'           chkcall
102800010202     C                   endif
102900010202     C*
103000010202     C                   ENDSR
103100141028     C*----------------------------------------------------*
103200141028     C*  CONTROLLO NUMERICITA' CAMPI
103300141028     C*----------------------------------------------------*
103400141028     C     CHKNUM        BEGSR
103500141028     C*
103600141028     C                   IF        PiDecChr = *blanks
103700141028     C                   EVAL      PiDecChr = CharNUM
103800141028     C                   ENDIF
103900141028     C*
104000141028     C                   callp     UBISNUM_Check(PiStr
104100141028     C                                          :PiDecChr
104200141028     C                                          :PiVal
104300141028     C                                          :PiNum
104400141028     C                                          :PiInt)
104500141028     C*
104600141028     C                   ENDSR
104700141028     C***
104800141028
104900141028
105000141028     C*----------------------------------------------------*
105100141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
105200141028     C*----------------------------------------------------*
105300141028     C     SR_SPLIT      BEGSR
105400141028     C*
105500141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
105600141028     C                   CALL      'XSPLIT2'
105700141028     C                   PARM                    InStringa
105800141028     C                   PARM                    InSepara
105900141028     C                   PARM      '5'           InTypeOut
106000141028     C                   PARM      ''            wSkParam
106100141028     C                   PARM                    OutErrore
106200141028     C                   PARM                    OutMsg
106300141028     C                   MOVEA     wSkParam      SkSplitCSV_5
106400141028     C*
106500141028     C                   ENDSR
106600141028
106700141028
106800141028     C*----------------------------------------------------*
106900141028     C*  SCRITTURA BUFFER EDIVAT
107000141028     C*----------------------------------------------------*
107100141028     C     WRIVAT        BEGSR
107200141028     C*
107300141028     C                   IF        VATNOT <> *blanks
107400141028     C                   EVAL      VATFGS = VABFGS
107500141028     C                   EVAL      VATCCM = VABCCM
107600141028     C                   EVAL      VATLNP = VABLNP
107700141028     C                   EVAL      VATAAS = VABAAS
107800141028     C                   EVAL      VATNRS = VABNRS
107900141028     C                   EVAL      VATNSP = VABNSP
108000141028     C                   EVAL      VATCMR = VABCMR
108100141028     C                   EVAL      VATCNT = VABCNT
108200141028     C                   ADD       1             §CTROKVT
108300141028     C                   WRITE     EDIVAT00
108400141028     C                   ENDIF
108500141028     C*
108600141028     C                   ENDSR
108700000801
108800011113
108900011113     C*----------------------------------------------------*
109000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
109100011113     C*----------------------------------------------------*
109200011113     C     CHKIMPDIV     BEGSR
109300011113     C*
109400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
109500011113     C                   Z-ADD     *zeros        wrkDec            9 9
109600011113     C*
109700011113     C* Come prima cosa effettuo considerazioni sulla divisa
109800011113     C                   IF        vabIAS > *zeros
109900011113     C                   IF        vabVAS <> 'EUR'
110000011113     C                   EVAL      vabVAS =  'ITL'
110100011113     C                   ENDIF
110200011113     C                   ENDIF
110300011113     C*
110400011113     C                   IF        vabCAS > *zeros
110500011113     C                   IF        vabVCA <> 'EUR'
110600011113     C                   EVAL      vabVCA =  'ITL'
110700011113     C                   ENDIF
110800011113     C                   ENDIF
110900011113     C*
111000011113     C                   IF        vabVMD > *zeros
111100020305     C                   IF        vabVAD <> 'EUR'
111200011113     C                   EVAL      vabVAD =  'ITL'
111300011113     C                   ENDIF
111400011113     C                   ENDIF
111500011113     C*
111600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
111700011113     C                   Z-ADD     vabIAS        wrkDec
111800011113     C                   IF        wrkDec > *zeros
111900011113     C                   IF        vabVAS = 'ITL'
112000011113     C                   EVAL      vabIAS = *zeros
112100011113     C                   ENDIF
112200011113     C                   ENDIF
112300011113     C*
112400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
112500011113     C                   Z-ADD     vabCAS        wrkDec
112600011113     C                   IF        wrkDec > *zeros
112700011113     C                   IF        vabVCA = 'ITL'
112800011113     C                   EVAL      vabCAS = *zeros
112900011113     C                   ENDIF
113000011113     C                   ENDIF
113100011113     C*
113200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
113300011113     C                   Z-ADD     vabVMD        wrkDec
113400011113     C                   IF        wrkDec > *zeros
113500011113     C                   IF        vabVAD = 'ITL'
113600011113     C                   EVAL      vabVMD = *zeros
113700011113     C                   ENDIF
113800011113     C                   ENDIF
113900011113     C*
114000011113     C                   ENDSR
114100011113     C***
114200011113
114300011113
114400000801
114500000801
114600990920      /TITLE Invio dei dati al punto operativo.
114700010202     C     invio         BEGSR
114800990920     C*
114900140117     C* 1° invio EDIVAT
115000010201     C                   reset                   dscmz
115100010201     C                   move      vlrpoi        cmzdst
115200140117     C                   eval      cmzfld = 'EDIVATWR'
115300010201     C                   eval      cmzmbd = vlrhdl
115400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
115500021009     C***                if        prmfir = *blanks
115600140117     C                   eval      cmzfla = 'EDIVAT0F'
115700140117     C                   eval      cmzmba = 'EDIVAT0F'
115800021009     C***                else
115900021009     C***                eval      cmzfla = prmfir
116000021009     C***                eval      cmzmba = prmfir
116100021009     C***                endif
116200010201     C                   eval      cmznrr = *zeros
116300020305     C                   move      §ctrokvt      cmznrr
116400021018     C                   eval      cmzlba = vlrfl1
116500010201     C                   call(e)   'TIS711C'
116600010201     C                   parm                    dscmz
116700010201     C                   parm      *blanks       esito
116800010205     C                   if        %error
116900010205     C                             or cmzerr = '1'
117000010205     C                             or esito  = '1'
117100010205     C                   eval      wrkesito = '3'
117200010205     C                   else
117300010201     C*
117400140117     C* 2° invio EDIVAB
117500010201     C                   reset                   dscmz
117600010201     C                   move      vlrpoi        cmzdst
117700010201     C                   eval      cmzfld = vlrfou
117800010201     C                   eval      cmzmbd = vlrhdl
117900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
118000021009     C***                if        prmfir = *blanks
118100140117     C                   eval      cmzfla = 'EDIVAB0F'
118200140117     C                   eval      cmzmba = 'EDIVAB0F'
118300021009     C***                else
118400021009     C***                eval      cmzfla = prmfir
118500021009     C***                eval      cmzmba = prmfir
118600021009     C***                endif
118700010201     C                   eval      cmznrr = *zeros
118800010201     C                   move      §ctrokvb      cmznrr
118900021018     C                   eval      cmzlba = vlrfl1
119000010201     C                   call(e)   'TIS711C'
119100010201     C                   parm                    dscmz
119200010201     C                   parm      *blanks       esito
119300010201     C                   if        %error
119400010201     C                             or cmzerr = '1'
119500010201     C                             or esito  = '1'
119600010201     C                   eval      wrkesito = '3'
119700010201     C                   endif
119800010205     C                   endif
119900990920     C*
120000000613     C                   ENDSR
120100000613     C***
120200070411
120300141028
120400070411     C     *pssr         BEGSR
120500070411     C*
120600070411     C                   if        %open(tivin00r)
120700070411     C                   close     tivin00r
120800070411     C                   endif
120900140117     C                   if        %open(edivabwr)
121000140117     C                   close     edivabwr
121100070411     C                   endif
121200140117     C                   if        %open(edivatwr)
121300140117     C                   close     edivatwr
121400070411     C                   endif
121500070411     C*
121600070411     C* Effettuo la chiamata al CLLE preposto
121700140117     C                   call(e)   'TITVEVTC'
121800070411     C                   parm                    parccm
121900070411     C                   parm                    parmbr
122000070411     C                   parm      '2'           paropz
122100070411     C*
122200070411     C                   eval      wrkesito = '2'
122300070411     C*
122400070411     C                   seton                                        LR
122500070411     C*
122600070411     C                   ENDSR     '*CANCL'
122700070411     C***
122800161018
122900161018
123000161018     C*--------------------------------------------------------
123100161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
123200161018     C*--------------------------------------------------------
123300161018     C     CARTAB        BEGSR
123400161018     C*
123500161018     C* TABELLA '15' - NAZIONI
123600161018     C                   clear                   skNAZISO
123700161018     C                   clear                   skNAZBAR
123800161018     C                   eval      tblKUT = 1
123900161018     C                   eval      tblCOD = '15'
124000161018     C     KEYtabP       setll     tabel00f
124100161018     C     KEYtabP       reade     tabel00f
124200161018     C                   dow       not %eof(tabel00f)
124300161018     C                   if        tblFLG = *blanks
124400161018     C                   movel(p)  tblUNI        ds15
124500161018     C                   add       1             jNAZ
124600161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
124700161018     C                   eval      skNAZISO(jNAZ) = §15COD
124800161018     C                   endif
124900161018     C     KEYtabP       reade     tabel00f
125000161018     C                   enddo
125100161018     C*
125200161018     C                   ENDSR
125300161018     C***
125400070411
125500990910
125600000613     C     *inzsr        BEGSR
125700990910     C*
125800990910     C     *entry        plist
125900990920     C                   parm                    tivlrds
126000990921     C                   parm      wrkesito      esito
126100000724     C                   parm                    prmlit
126200000710     C                   parm                    prmfir
126300000613     C*
126400000830     C* CALCOLA LA DATA CORRENTE
126500140117     C                   time                    wn14             14 0
126600140117     C                   movel     wn14          oracor            6 0          *ORA
126700100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
126800100722     C                   eval      datcor = %dec(%date() : *ISO)
126900161018     C*
127000161018     C* Chiave su TABEL00F - parziale
127100161018     C     KEYtabP       KLIST
127200161018     C                   KFLD                    tblKUT
127300161018     C                   KFLD                    tblCOD
127400000830     C*
127500000613     C                   ENDSR
127600000613     C***
