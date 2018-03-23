000100180115      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVADWR/EDIVAD1WR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400161018     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140117     FEDIVABwr  O    E             DISK    usropn
000700171218     FEDIVATwr  O    E             DISK    usropn
000800180116     FEDIVAD1wr O    E             DISK    usropn prefix(H_)
000900180115     FEDIVADwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400180116     D***EDIVAD1wr      e ds                         prefix(H_)
001500180116
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070719     D tisi95ds      e ds
002100140117     D edivabds      e ds                  extname(edivab0f)
002200161018     D ds15          e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200141028     D wTag            s            128    VARYING INZ
003300180124     D wRecErrBlk      s              1  0 INZ(*zeros)
003400180124     D wRecErrNoBlk    s              1  0 INZ(*zeros)
003500141028     D wFlgCAS         s              1    INZ(*blanks)
003600141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003700150527     D wNOTE           s             70    INZ(*blanks)
003800161013     D wEMAIL          s             70    INZ(*blanks)
003900141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
004000141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
004100141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
004200141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
004300141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
004400141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
004500141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
004600171108     D wI              s              3s 0
004700171220     D wJ              s              3s 0
004800171218     D wRFF            s              3  0 INZ(*zeros)
004900161018
005000161018     D*------------
005100161018     D jNAZ            s              5  0 INZ(*zeros)
005200161018     D skNAZISO        S              3    DIM(1000)
005300161018     D skNAZBAR        S              3    DIM(1000)
005400141028
005500141028     D*------------------
005600141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005700141028     D*------------------
005800141028     D CharNUM         S              1
005900141028     D CharSOS         S              1
006000141028     D posDaDft        S              3  0 INZ(*zeros)
006100141028     D posADft         S              3  0 INZ(*zeros)
006200141028     D wGiroDft        s              1  0 INZ(*zeros)
006300141028
006400141028
006500141028     D*------------------
006600141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006700141028     D*------------------
006800141028     D InStringa       S          65535A   VARYING                              (stringa input)
006900141028     D InSepara        S             10A                                        (separatore)
007000141028     D InTypeOut       S              1A                                        (tipo output)
007100141028     D wSkParam        S          65535A                                        (output)
007200141028     D OutErrore       S              1A                                        (flag errore)
007300141028     D OutMsg          S             80A                                        (messaggio errore)
007400141028     D SkSplitCSV_5    S            256    DIM(256)
007500171220     D SkSplitCSV_Sav  S            256    DIM(256)
007600180116     D SkRFF           S             35    DIM(100)
007700141028
007800000830
007900041025     D*------------------
008000041025     D* DS REPERIMENTO NUMERATORE
008100041025     D*------------------
008200041025     D trul33ds      e ds                  inz
008300041025     D*------------------
008400041025     D* DS ARCHITETTURA
008500041025     D*------------------
008600041025     D kpjba         e ds                  inz
008700041025     D*------------------
008800990908
008900141028
009000141028     D*------------------
009100141028     D* LINKING A DEFINIZIONI ESTERNE
009200141028     D*------------------
009300141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009500150609
009600141028
009700150609     D*-------------------
009800150609     D* COSTANTI
009900150609     D*-------------------
010000150609     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010100150609     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010200141028
010300010201
010400010201
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800000613     C*
010900161018     C                   EXSR      CARTAB                                       CARICA TABELLE
011000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
011100070719     C*
011200070719     C* Esegue lancio TISI95R solo x chiusura
011300070719     C                   CLEAR                   TISI95DS
011400070719     C                   EVAL      I95TLA = 'C'
011500070719     C                   CALL      'TISI95R'
011600070719     C                   PARM                    TISI95DS
011700000613     C*
011800010202     C* Effettuo la chiamata al CLLE preposto
011900180116     C                   call(e)   'TITVEVD1TC'
012000180116     C***lk              call(e)   'TITVEVDTC'
012100140922     C                   parm                    parccm
012200140922     C                   parm                    parmbr
012300140922     C                   parm      '2'           paropz
012400170731     C*
012500170731     C                   if        *in53
012600170731     C                   call      'TIS7P2SER'
012700170731     C                   parm      'C'           pIn_Opz           1
012800170731     C                   parm                    tivlrds
012900170731     C                   parm                    EDIVABDS
013000170731     C                   parm      *blanks       pIn_CdIdAz        2
013100170731     C                   parm      *blanks       pIn_MaskPDF      50
013200170731     C                   parm      *blanks       pOut_Esito        1
013300170731     C                   endif
013400000616     C*
013500010201     C                   seton                                        LR
013600990908
013700000801
013800910830     C*--------------------------------------------------------
013900171219     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR EDIVATWR e EDIVAD1WR   *
014000910830     C*--------------------------------------------------------
014100040526     C     RWFILE        BEGSR
014200990910     C*
014300990914     C                   if        not %open(tivin00r)
014400990908     C                   open      tivin00r
014500990914     C                   endif
014600140117     C                   if        not %open(edivabwr)
014700140117     C                   open      edivabwr
014800171219     C                   endif
014900171219     C*
015000180115     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR/EDIVADWR/EDIVAD1WR
015100171219     C                   exsr      prevaD1T
015200171219     C*
015300171219     C                   if        chkcall = '0'
015400171219     C*
015500180116     C                   if        not %open(EDIVAD1wr)
015600180116     C                   open      EDIVAD1wr
015700180116     C                   endif
015800180116     C                   if        not %open(EDIVADwr)
015900180115     C                   open      EDIVADwr
016000180115     C                   endif
016100171219     C                   if        not %open(EDIVATwr)
016200171219     C                   open      EDIVATwr
016300010201     C                   endif
016400990910     C*
016500010201     C                   clear                   §CTROKVB          5 0
016600020305     C                   clear                   §CTROKVT          5 0
016700180116     C                   clear                   §CTROKVD          5 0
016800180116     C                   clear                   §CTROKVD1         5 0
016900000801     C                   clear                   §CTRMO            5 0
017000000801     C                   clear                   §CTRNO            5 0
017100141028     C*
017200141028     C* Inizializzazioni fuori ciclo
017300141028     C                   EXSR      INZVAR
017400140117     C*
017500140902     C                   if        *in53
017600140117     C                   call      'TIS7P2SER'
017700140117     C                   parm      'O'           pIn_Opz           1
017800140117     C                   parm                    tivlrds
017900140117     C                   parm                    EDIVABDS
018000140117     C                   parm      *blanks       pIn_CdIdAz        2
018100140117     C                   parm      *blanks       pIn_MaskPDF      50
018200140117     C                   parm      *blanks       pOut_Esito        1
018300140902     C                   endif
018400130708     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800040910     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000000801     C*
019100000801     C                   if        *in70 = *off
019200000801     C                             and
019300000801     C                             (vinflg = *blanks
019400000801     C                              or vinflg = '0'
019500000801     C                              or vinflg = '2')
019600000801     C*
019700000801     C                   clear                   vinmsg
019800141028     C*
019900141028     C* "normalizzo" il dato di input
020000141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
020100141028     C                                             %len(%trim(vindta))-1))
020200160209     C*
020300160209     C* Elimino le "duple di controllo EDIFACT"
020400160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
020500160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
020600160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
020700160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
020800160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
020900040910     C*
021000040910     C* Eseguo routine d traduzione
021100171218     C                   exsr      impvabvad
021200040802     C*
021300010305     C                   endif
021400000905     C*
021500000905     C                   else
021600000905     C                   eval      vinflg = '1'
021700000905     C                   endif
021800000905     C*
021900000905     C  N70              update    tivin000
022000000905     C*
022100991022     C  N70              ENDdo
022200010202     C*
022300010202     C                   endif
022400990910
022500990910     C* Se non ci sono record con errori ...
022600000710     C                   if        §ctrno = 0
022700990910     C* ... restituisco esito OK.
022800990921     C                   eval      wrkesito = '0'
022900990910     C                   else
023000010201     C                   if        §ctrokvb > 0
023100990921     C                   eval      wrkesito = '1'
023200000710     C                   else
023300000710     C                   eval      wrkesito = '2'
023400990910     C                   endif
023500000710     C                   endif
023600990910     C*
023700990914     C                   if        %open(tivin00r)
023800990908     C                   close     tivin00r
023900990914     C                   endif
024000140117     C                   if        %open(edivabwr)
024100140117     C                   close     edivabwr
024200990914     C                   endif
024300180116     C                   if        %open(EDIVAD1wr)
024400180116     C                   close     EDIVAD1wr
024500180116     C                   endif
024600180115     C                   if        %open(EDIVADwr)
024700180115     C                   close     EDIVADwr
024800180115     C                   endif
024900171219     C                   if        %open(EDIVATwr)
025000171219     C                   close     EDIVATwr
025100171219     C                   endif
025200990910     C*
025300010201     C                   if        §ctrokvb > 0
025400000724     C                             and vlrpoi <> *zeros
025500180116     C                   exsr      invio
025600990920     C                   endif
025700990920     C*
025800910830     C                   ENDSR
025900000613     C***
026000140922
026100140922     C*----------------------------------------------------*
026200140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
026300140922     C*----------------------------------------------------*
026400140922     C     CHKWRI        BEGSR
026500140922     C*
026600141028     C                   if        VABRMN = *zeros
026700141028     C                   eval      VABRMN = VABNSP
026800180124     C                   endif
026900170214     C*
027000171031     C* Se peso non indicato nei dati cliente e richiesto un peso forzato per collo
027100170214     C                   IF        VABPKB = *zeros
027200170214     C                   EVAL      VABPKB = wVABPKN * VABNCL
027300170214     C                   ENDIF
027400141028     C*
027500141028     C                   if        VABPRD = *blanks
027600141028     C* ......VABPRD
027700141028     C* Reperisco la provincia dal CAP e dalla località
027800141028     C                   IF        VABLOD <> *blanks AND
027900141028     C                             VABCAD <> *blanks AND
028000141028     C                             VABNZD  = *blanks
028100141028     C                   CLEAR                   TISI95DS
028200141028     C                   EVAL      I95TCN = '3'
028300141028     C                   Z-ADD     datcor        I95DAT
028400141028     C                   EVAL      I95CAP = VABCAD
028500141028     C                   EVAL      I95LOC = VABLOD
028600141028     C                   CALL      'TISI95R'
028700141028     C                   PARM                    TISI95DS
028800141028     C                   EVAL      VABPRD = O95PRV
028900141028     C                   ENDIF
029000141028     C                   endif
029100140922     C*
029200140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
029300141028     C                   IF        wFlgCAS <> '0'
029400140922     C                   IF        VABCBO = '1'
029500140922     C                   EVAL      VABCBO = '4'
029600140922     C                   ELSE
029700140922     C                   EVAL      VABCBO = '6'
029800140922     C                   ENDIF
029900140922     C                   ENDIF
030000140922     C*
030100140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030200141028     C                   EXSR      CHKIMPDIV
030300150527     C*
030400150527     C* Scompongo le note
030500150527     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
030600150527     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
030700140922     C*
030800140922     C                   ENDSR
030900141028     C*----------------------------------------------------*
031000141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031100141028     C*----------------------------------------------------*
031200141028     C     INZVAR        BEGSR
031300141028     C*
031400141028     C* Inizializzo variabili di wrk
031500141028     C                   Z-ADD     *zeros        Num5_0            5 0
031600141028     C                   MOVEL     '0'           wFlgCAS
031700141028     C                   MOVEL     *blanks       wVATNOT_A
031800141028     C                   MOVEL     *blanks       wVATNOT_B
031900141028     C                   MOVEL     *blanks       wVATNOT_E
032000141028     C                   MOVEL     *blanks       wVATNOT_I
032100141028     C                   MOVEL     *blanks       wVATNOT_J
032200141028     C                   MOVEL     *blanks       wVATNOT_S
032300141028     C                   MOVEL     *blanks       wVATNOT_P
032400141028     C                   CLEAR                   SkSplitCSV_5
032500150527     C                   MOVEL     *blanks       wNOTE
032600180116     C                   CLEAR                   SkRFF
032700180116     C                   CLEAR                   wRFF
032800141028     C*
032900141028     C* Reimposto i valori di default
033000141028     C                   EXSR      DEFCAM
033100141028     C*
033200141028     C                   ENDSR
033300141028     C*----------------------------------------------------*
033400141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
033500141028     C*----------------------------------------------------*
033600141028     C     DEFCAM        BEGSR
033700141028     C*
033800141028     C                   CLEAR                   EDIVAB00
033900171219     C                   CLEAR                   EDIVAT00
034000180115     C                   CLEAR                   EDIVAD00
034100180116     C*                  CLEAR                   EDIVAD100
034200141028     C*
034300141028     C* Imposto i valori bolla di default
034400141028     C                   EVAL      VABCTR = 000
034500141028     C                   EVAL      VABCBO = '1'
034600141104     C                   EVAL      VABTSP = 'C'
034700141028     C*
034800141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
034900141028     C* e delimitatore testo.
035000141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
035100141028     C*
035200141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
035300141028     C                   EVAL      CharSOS = CharNUM
035400141028     C*
035500141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
035600141028     C                   SETOFF                                       53
035700141028     C                   SELECT
035800141028     C                   WHEN      %subst(vlrppt:1:3) = 'PDF'
035900141028     C                   SETON                                        53
036000141028     C                   ENDSL
036100141028     C*
036200141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
036300141028     C                   EVAL      posDaDft = 1
036400141028     C                   EVAL      posADft  = 0
036500141028     C                   EVAL      wGiroDft = 0
036600141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
036700141028     C                             posDaDft > 0
036800141028     C*
036900141028     C* Gestisco il 1° giro
037000141028     C                   IF        wGiroDft = 0
037100141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
037200141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
037300141028     C* Incremento il contatore dei "giri"
037400141028     C                   EVAL      wGiroDft = 1
037500141028     C                   ELSE
037600141028     C                   EVAL      posDaDft = posADft
037700141028     C                   ENDIF
037800141028     C* Eseguo lo scan x trovare la fine del campo corrente
037900141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
038000141028     C*
038100141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
038200141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
038300141028     C                   IF        posDaDft > 0 AND
038400141028     C                             posADft  > 0
038500141028     C* NCL
038600141028     C                   IF        %subst(
038700141028     C                             %subst(vlrppt:posDaDft+1:
038800141028     C                             posADft-posDaDft-1):1:3)
038900141028     C                             = 'NCL'
039000141028     C                   EVAL      PiStr=%trim(%subst(
039100141028     C                             %subst(vlrppt:posDaDft+1:
039200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039300141028     C                   EXSR      CHKNUM
039400141028     C                   IF        PiInt=*on
039500141028     C                   Z-ADD     PiVal         VABNCL
039600141028     C                   ENDIF
039700141028     C                   ENDIF
039800141028     C* CCM
039900141028     C                   IF        %subst(
040000141028     C                             %subst(vlrppt:posDaDft+1:
040100141028     C                             posADft-posDaDft-1):1:3)
040200141028     C                             = 'CCM'
040300141028     C                   EVAL      PiStr=%trim(%subst(
040400141028     C                             %subst(vlrppt:posDaDft+1:
040500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040600141028     C                   EXSR      CHKNUM
040700141028     C                   IF        PiInt=*on
040800171219     C                   Z-ADD     PiVal         VABCCM
040900141028     C                   ENDIF
041000141028     C                   ENDIF
041100141028     C* LNP
041200141028     C                   IF        %subst(
041300141028     C                             %subst(vlrppt:posDaDft+1:
041400141028     C                             posADft-posDaDft-1):1:3)
041500141028     C                             = 'LNP'
041600141028     C                   EVAL      PiStr=%trim(%subst(
041700141028     C                             %subst(vlrppt:posDaDft+1:
041800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041900141028     C                   EXSR      CHKNUM
042000141028     C                   IF        PiInt=*on
042100141028     C                   Z-ADD     PiVal         VABLNP
042200141028     C                   ENDIF
042300141028     C                   ENDIF
042400141028     C* NRS
042500141028     C                   IF        %subst(
042600141028     C                             %subst(vlrppt:posDaDft+1:
042700141028     C                             posADft-posDaDft-1):1:3)
042800141028     C                             = 'NRS'
042900141028     C                   EVAL      PiStr=%trim(%subst(
043000141028     C                             %subst(vlrppt:posDaDft+1:
043100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043200141028     C                   EXSR      CHKNUM
043300141028     C                   IF        PiInt=*on
043400141028     C                   Z-ADD     PiVal         VABNRS
043500141028     C                   ENDIF
043600141028     C                   ENDIF
043700141028     C* CTR
043800141028     C                   IF        %subst(
043900141028     C                             %subst(vlrppt:posDaDft+1:
044000141028     C                             posADft-posDaDft-1):1:3)
044100141028     C                             = 'CTR'
044200141028     C                   EVAL      PiStr=%trim(%subst(
044300141028     C                             %subst(vlrppt:posDaDft+1:
044400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044500141028     C                   EXSR      CHKNUM
044600141028     C                   IF        PiInt=*on
044700141028     C                   Z-ADD     PiVal         VABCTR
044800141028     C                   ENDIF
044900141028     C                   ENDIF
045000141028     C* PKB
045100141028     C                   IF        %subst(
045200141028     C                             %subst(vlrppt:posDaDft+1:
045300141028     C                             posADft-posDaDft-1):1:3)
045400141028     C                             = 'PKB'
045500141028     C                   EVAL      PiStr=%trim(%subst(
045600141028     C                             %subst(vlrppt:posDaDft+1:
045700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045800141028     C                   EXSR      CHKNUM
045900141028     C                   IF        PiNum=*on
046000141028     C                   Z-ADD     PiVal         VABPKB
046100141028     C                   ENDIF
046200141028     C                   ENDIF
046300141028     C* VLB
046400141028     C                   IF        %subst(
046500141028     C                             %subst(vlrppt:posDaDft+1:
046600141028     C                             posADft-posDaDft-1):1:3)
046700141028     C                             = 'VLB'
046800141028     C                   EVAL      PiStr=%trim(%subst(
046900141028     C                             %subst(vlrppt:posDaDft+1:
047000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047100141028     C                   EXSR      CHKNUM
047200141028     C                   IF        PiNum=*on
047300141028     C                   Z-ADD     PiVal         VABVLB
047400141028     C                   ENDIF
047500141028     C                   ENDIF
047600141028     C* QFT
047700141028     C                   IF        %subst(
047800141028     C                             %subst(vlrppt:posDaDft+1:
047900141028     C                             posADft-posDaDft-1):1:3)
048000141028     C                             = 'QFT'
048100141028     C                   EVAL      PiStr=%trim(%subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400141028     C                   EXSR      CHKNUM
048500141028     C                   IF        PiNum=*on
048600141028     C                   Z-ADD     PiVal         VABQFT
048700141028     C                   ENDIF
048800141028     C                   ENDIF
048900141028     C* CBO
049000141028     C                   IF        %subst(
049100141028     C                             %subst(vlrppt:posDaDft+1:
049200141028     C                             posADft-posDaDft-1):1:3)
049300141028     C                             = 'CBO'
049400141028     C                   EVAL      VABCBO=%trim(%subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700141028     C                   ENDIF
049800141028     C* TSP
049900141028     C                   IF        %subst(
050000141028     C                             %subst(vlrppt:posDaDft+1:
050100141028     C                             posADft-posDaDft-1):1:3)
050200141028     C                             = 'TSP'
050300141028     C                   EVAL      VABTSP=%trim(%subst(
050400141028     C                             %subst(vlrppt:posDaDft+1:
050500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600141028     C                   ENDIF
050700141028     C* VAS
050800141028     C                   IF        %subst(
050900141028     C                             %subst(vlrppt:posDaDft+1:
051000141028     C                             posADft-posDaDft-1):1:3)
051100141028     C                             = 'VAS'
051200141028     C                   EVAL      VABVAS=%trim(%subst(
051300141028     C                             %subst(vlrppt:posDaDft+1:
051400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500141028     C                   ENDIF
051600141028     C* VCA
051700141028     C                   IF        %subst(
051800141028     C                             %subst(vlrppt:posDaDft+1:
051900141028     C                             posADft-posDaDft-1):1:3)
052000141028     C                             = 'VCA'
052100141028     C                   EVAL      VABVCA=%trim(%subst(
052200141028     C                             %subst(vlrppt:posDaDft+1:
052300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400141028     C                   ENDIF
052500141028     C* TIC
052600141028     C                   IF        %subst(
052700141028     C                             %subst(vlrppt:posDaDft+1:
052800141028     C                             posADft-posDaDft-1):1:3)
052900141028     C                             = 'TIC'
053000141028     C                   EVAL      VABTIC=%trim(%subst(
053100141028     C                             %subst(vlrppt:posDaDft+1:
053200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300141028     C                   ENDIF
053400141028     C* GCA
053500141028     C                   IF        %subst(
053600141028     C                             %subst(vlrppt:posDaDft+1:
053700141028     C                             posADft-posDaDft-1):1:3)
053800141028     C                             = 'GCA'
053900141028     C                   EVAL      VABGCA=%trim(%subst(
054000141028     C                             %subst(vlrppt:posDaDft+1:
054100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200141028     C                   ENDIF
054300141028     C* CTM
054400141028     C                   IF        %subst(
054500141028     C                             %subst(vlrppt:posDaDft+1:
054600141028     C                             posADft-posDaDft-1):1:3)
054700141028     C                             = 'CTM'
054800141028     C                   EVAL      VABCTM=%trim(%subst(
054900141028     C                             %subst(vlrppt:posDaDft+1:
055000171218     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100141028     C                   ENDIF
055200141028     C* FFD
055300141028     C                   IF        %subst(
055400141028     C                             %subst(vlrppt:posDaDft+1:
055500141028     C                             posADft-posDaDft-1):1:3)
055600141028     C                             = 'FFD'
055700141028     C                   EVAL      VABFFD=%trim(%subst(
055800141028     C                             %subst(vlrppt:posDaDft+1:
055900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000141028     C                   ENDIF
056100141028     C* VAD
056200141028     C                   IF        %subst(
056300141028     C                             %subst(vlrppt:posDaDft+1:
056400141028     C                             posADft-posDaDft-1):1:3)
056500141028     C                             = 'VAD'
056600141028     C                   EVAL      VABVAD=%trim(%subst(
056700141028     C                             %subst(vlrppt:posDaDft+1:
056800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900141028     C                   ENDIF
057000141028     C* GMA
057100141028     C                   IF        %subst(
057200141028     C                             %subst(vlrppt:posDaDft+1:
057300141028     C                             posADft-posDaDft-1):1:3)
057400141028     C                             = 'GMA'
057500141028     C                   EVAL      VABGMA=%trim(%subst(
057600141028     C                             %subst(vlrppt:posDaDft+1:
057700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800141028     C                   ENDIF
057900141028     C* GGA
058000141028     C                   IF        %subst(
058100141028     C                             %subst(vlrppt:posDaDft+1:
058200141028     C                             posADft-posDaDft-1):1:3)
058300141028     C                             = 'GGA'
058400141028     C                   EVAL      VABGGA=%trim(%subst(
058500141028     C                             %subst(vlrppt:posDaDft+1:
058600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700141028     C                   ENDIF
058800141028     C* GVA
058900141028     C                   IF        %subst(
059000141028     C                             %subst(vlrppt:posDaDft+1:
059100141028     C                             posADft-posDaDft-1):1:3)
059200141028     C                             = 'GVA'
059300141028     C                   EVAL      VABGVA=%trim(%subst(
059400141028     C                             %subst(vlrppt:posDaDft+1:
059500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600141028     C                   ENDIF
059700141028     C* TC1
059800141028     C                   IF        %subst(
059900141028     C                             %subst(vlrppt:posDaDft+1:
060000141028     C                             posADft-posDaDft-1):1:3)
060100141028     C                             = 'TC1'
060200141028     C                   EVAL      VABTC1=%trim(%subst(
060300141028     C                             %subst(vlrppt:posDaDft+1:
060400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500141028     C                   ENDIF
060600141028     C* TC2
060700141028     C                   IF        %subst(
060800141028     C                             %subst(vlrppt:posDaDft+1:
060900141028     C                             posADft-posDaDft-1):1:3)
061000141028     C                             = 'TC2'
061100141028     C                   EVAL      VABTC2=%trim(%subst(
061200141028     C                             %subst(vlrppt:posDaDft+1:
061300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400141028     C                   ENDIF
061500141028     C* VATTRC
061600141028     C                   IF        %subst(
061700141028     C                             %subst(vlrppt:posDaDft+1:
061800141028     C                             posADft-posDaDft-1):1:3)
061900141028     C                             = 'TRC'
062000141028     C                   EVAL      VATTRC=%trim(%subst(
062100141028     C                             %subst(vlrppt:posDaDft+1:
062200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300141028     C                   ENDIF
062400170214     C*
062500170214     C***
062600170214     C* Campi "personalizzati"
062700170214     C***
062800170214     C* PKN
062900170214     C                   IF        %subst(
063000170214     C                             %subst(vlrppt:posDaDft+1:
063100170214     C                             posADft-posDaDft-1):1:3)
063200170214     C                             = 'PKN'
063300170214     C                   EVAL      PiStr=%trim(%subst(
063400170214     C                             %subst(vlrppt:posDaDft+1:
063500170214     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600170214     C                   EXSR      CHKNUM
063700170214     C                   IF        PiNum=*on
063800170214     C                   Z-ADD     PiVal         wVABPKN           7 1
063900170214     C                   ENDIF
064000170214     C                   ENDIF
064100170214     C***
064200141028     C* ...
064300141028     C                   ENDIF
064400141028     C                   ENDDO
064500141028     C*
064600141028     C                   ENDSR
064700000801     C*----------------------------------------------------*
064800180115     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB EDIVAT EDIVAD EDIVAD10)
064900000801     C*----------------------------------------------------*
065000171218     C     IMPVABVAD     BEGSR
065100040910     C*
065200040910     C* Traduzione relativa ai tipi record del file del cliente
065300180124     C*   gli errori bloccanti hanno significato per l'intera spedizione
065400180124     C*   gli errori non bloccanti hanno significato per ogni record della spedizione
065500180124     C                   Z-ADD     0             wRecErrNoBlk
065600180124     C*
065700071210     C***
065800141028     C* ...tipo record 'UNB' (info CMR)
065900141028     C                   EVAL      wTag = 'UNB'
066000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
066100141028     C                   MOVEL     *blanks       savCMR           35
066200141028     C                   EVAL      InStringa =
066300141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
066400180212     C* così ottengo una schiera di 5 elementi
066500180111     C                   EVAL      InSepara = '+'
066600141028     C                   EXSR      SR_SPLIT
066700180212     C                   EVAL      InStringa =
066800180212     C                                %trim(SkSplitCSV_5(4))
066900180212     C* così ottengo una schiera di 2 elementi con 1) data sped. Difarco 2) nr. spe Difarco
067000180212     C                   EVAL      InSepara = ':'
067100180212     C                   EXSR      SR_SPLIT
067200180212     C                   EVAL      savCMR = %trim(SkSplitCSV_5(2)) + ' '+
067300140922     C                                      %char(datcor) + ' ' + %char(oracor)
067400130620     C                   ENDIF
067500141028     C*
067600141028     C***
067700161025     C* ...tipo record 'UNH' (rottuta di codice spedizione)
067800161025     C                   EVAL      wTag = 'UNH'
067900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
068000150526     C*
068100150526     C* Inizializzazioni per nuova spedizione
068200180124     C*   gli errori bloccanti hanno significato per l'intera spedizione
068300180124     C*   gli errori non bloccanti hanno significato per ogni record della spedizione
068400180124     C                   Z-ADD     0             wRecErrBlk
068500141028     C                   eval      vinflg = '1'
068600141028     C* ......inizializzazioni iniziali e formati record file Bartolini
068700141028     C                   EXSR      INZVAR
068800141028     C* ......valorizzazione campi fissi
068900141028     C                   MOVEL     datcor        VABAAS
069000141028     C                   MOVE      datcor        VABMGS
069100141028     C                   MOVE(P)   vlrpoi        VABFGS
069200141028     C                   EVAL      VABCMR = savCMR
069300141028     C                   EVAL      VABDCM = datcor
069400141028     C                   EVAL      VABDTS = datcor
069500141028     C                   EVAL      VABCNT = 1
069600141028     C* ......NSP => Stacco un numeratore da AZNUM
069700141028     C                   clear                   TRUL33DS
069800141028     C                   eval      I33OPE = *zeros
069900141028     C                   eval      I33CNU = 302
070000141028     C                   eval      I33NUM = 1
070100141028     C                   movel     TRUL33DS      KPJBU
070200141028     C                   call      'TRUL33R'
070300141028     C                   parm                    KPJBA
070400141028     C                   movel     KPJBU         TRUL33DS
070500141028     C                   if        O33ERR = *zeros
070600141028     C                   z-add     O33NRF        VABNSP
070700141028     C                   else
070800180124     C                   Z-ADD     1             wRecErrBlk
070900141028     C                   EVAL      vinmsg = %trimr(vinmsg)
071000141028     C                             + ' ' + 'VABNSP VATNSP'
071100141028     C                   endif
071200180221     C* ......RMN => è la DN madre che Difarco scrive in questo segmento
071300180221     C*        sarà presente anche nei segmenti RFF
071400180221     C                   EVAL      InStringa =
071500180221     C                                %subst(wvindta:%len(%trim(wTag))+2)
071600180221     C                   EVAL      InSepara = '+'
071700180221     C                   EXSR      SR_SPLIT
071800180221     C* siccome è un dato numerico, controllo che non ci sia valore alfanumerico mediante MONITOR
071900180221     C                   MONITOR
072000180221     C                   EVAL      VABRMN = %dec(%trim(SkSplitCSV_5(1))
072100180221     C                                      : 15 : 0)
072200180305     C                   EVAL      VABRMA = %editc(VABRMN:'X')
072300180221     C                   ON-ERROR
072400180221     C                   Z-ADD     1             wRecErrBlk
072500180221     C                   EVAL      vinmsg = %trimr(vinmsg)
072600180221     C                             + ' ' + 'VABRMN'
072700180221     C                   ENDMON
072800141028     C                   ENDIF
072900161019     C*
073000161019     C***
073100170208     C* ...tipo record 'BGM' (riferimento spedizione)
073200180305     C* 20180301: si è deciso che VABRMA è = VABRMN
073300180305     C***                EVAL      wTag = 'BGM+610'
073400180305     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
073500180305     C***                EVAL      InStringa =
073600180305     C***                             %subst(wvindta:%len(%trim(wTag))+2)
073700180305     C***                EVAL      InSepara = '+'
073800180305     C***                EXSR      SR_SPLIT
073900180305     C***                EVAL      VABRMA = %trim(SkSplitCSV_5(1))
074000180305     C***                ENDIF
074100150527     C*
074200150527     C***
074300171031     C* ...tipo record 'DTM+2' (data consegna richiesta)
074400171031     C                   EVAL      wTag = 'DTM+2'
074500150527     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
074600150527     C                   EVAL      PiStr=%subst(
074700150527     C                               %subst(wvindta:%len(%trim(wTag))+2):1:8)
074800150527     C                   EXSR      CHKNUM
074900150527     C                   IF        PiInt=*on
075000150527     C                   Z-ADD     PiVal         VABDCR
075100150527     C                   ELSE
075200180124     C                   Z-ADD     1             wRecErrBlk
075300180111     C                   EVAL      vinmsg = %trimr(vinmsg)
075400180111     C                             + ' ' + 'VABDCR'
075500150527     C                   ENDIF
075600150527     C                   ENDIF
075700161025     C*
075800161025     C***
075900171031     C* ...tipo record 'FTX+DEL' (note di consegna)
076000171122     C                   EVAL      wTag = 'FTX+DEL'
076100171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076200171031     C                   EVAL      InStringa =
076300171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
076400171031     C                   EVAL      InSepara = '+'
076500171031     C                   EXSR      SR_SPLIT
076600171031     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
076700171031     C                                     %trim(SkSplitCSV_5(3))
076800171031     C                   ENDIF
076900171218     C*
077000171218     C***
077100171218     C* ...tipo record 'FTX+DIN' (particolarità varie / prenotazione telefonica)
077200171219     C                   EVAL      wTag = 'FTX+DIN'
077300171218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
077400171218     C                   EVAL      InStringa =
077500171218     C                                %subst(wvindta:%len(%trim(wTag))+2)
077600171218     C                   EVAL      InSepara = '+'
077700171218     C                   EXSR      SR_SPLIT
077800171218     C                   SELECT
077900171218     C                   WHEN      %trim(SkSplitCSV_5(3)) = 'U'
078000171218     C                   EVAL      VABGVA = %trim(SkSplitCSV_5(3))
078100171218     C                   WHEN      %trim(SkSplitCSV_5(3)) <> *blank
078200171219     C* richiesto appuntamento
078300171219     C                   EVAL      VABTC1 = 'A'
078400171219     C* telefono
078500180124     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV_5(3))
078600171218     C                   ENDSL
078700171218     C                   ENDIF
078800180223     C*
078900180223     C***
079000180223     C* ...tipo record 'FTX+EST' (note di consegna)
079100180223     C                   EVAL      wTag = 'FTX+EST'
079200180223     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079300180223     C                   EVAL      InStringa =
079400180223     C                                %subst(wvindta:%len(%trim(wTag))+2)
079500180223     C                   EVAL      InSepara = '+'
079600180223     C                   EXSR      SR_SPLIT
079700180223     C                   EVAL      VABNAS =
079800180223     C                                     %trim(SkSplitCSV_5(3))
079900180223     C                   ENDIF
080000171031     C*
080100171031     C***
080200171031     C* ...tipo record 'NAD+CZ' (mittente originale)
080300171031     C                   EVAL      wTag = 'NAD+CZ'
080400171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080500171031     C                   EVAL      InStringa =
080600171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
080700171031     C                   EVAL      InSepara = '+'
080800171031     C                   EXSR      SR_SPLIT
080900171031     C                   EVAL      VABCMO = %trim(SkSplitCSV_5(7))
081000171031     C                   EVAL      VABNMO = %trim(SkSplitCSV_5(8))
081100171031     C                   IF        VABNMO = 'I'   OR
081200171031     C                             VABNMO = 'IT'  OR
081300171031     C                             VABNMO = 'ITA'
081400171031     C                   EVAL      VABNMO = *blanks
081500171031     C                   ELSE
081600171031     C                   Z-ADD     1             jNAZ
081700171031     C     VABNMO        LOOKUP    skNAZISO(jNAZ)                         13
081800171031     C                   IF        %found
081900171031     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
082000171031     C                   ENDIF
082100171031     C                   ENDIF
082200171031     C*
082300171031     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
082400171031     C                   EVAL      InSepara = ':'
082500171031     C                   EXSR      SR_SPLIT
082600171031     C                   EVAL      VABRMO = %trim(SkSplitCSV_5(1))
082700171031     C                   ENDIF
082800171031     C*
082900171218     C***
083000171031     C* ...tipo record 'NAD+CN' (destinatario)
083100171031     C                   EVAL      wTag = 'NAD+CN'
083200171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083300171031     C                   EVAL      InStringa =
083400171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
083500171031     C                   EVAL      InSepara = '+'
083600171031     C                   EXSR      SR_SPLIT
083700180123     C                   EVAL      VABIND = %trim(SkSplitCSV_5(4))
083800171031     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
083900180124     C                   EVAL      PiStr = %trim(SkSplitCSV_5(7))
084000180124     C                   EXSR      CHKNUM
084100180124     C                   IF        PiInt=*on
084200180124     C                   Z-ADD     PiVal         Num5_0
084300180124     C                   MOVEL(p)  Num5_0        VABCAD
084400180124     C                   ELSE
084500180124     C                   Z-ADD     1             wRecErrNoBlk
084600180124     C                   EVAL      VABCAD = *zeros
084700180124     C                   EVAL      vinmsg = %trimr(vinmsg)
084800180124     C                             + ' ' + 'VABCAD'
084900180124     C                   ENDIF
085000180124     C                   EVAL      VABPRD = %trim(SkSplitCSV_5(6))
085100171031     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
085200171031     C                   IF        VABNZD = 'I'   OR
085300171031     C                             VABNZD = 'IT'  OR
085400171031     C                             VABNZD = 'ITA'
085500171031     C                   EVAL      VABNZD = *blanks
085600171031     C                   ENDIF
085700180313     C* se la nazione non è Italia
085800180313     C                   IF        VABNZD <>*blanks
085900180313     C* dato per scontato che al 2018/03/13 il flusso estero è DPD, non dobbiamo accorpare
086000180313     C                   EVAL      VABATB = 'D'
086100180313     C                   ENDIF
086200180215
086300180215     C* mi salvo la schiera prima che sia splittata
086400180215     C                   EVAL      SkSplitCSV_Sav = SkSplitCSV_5
086500180215
086600180215     C                   EVAL      InStringa = %trim(SkSplitCSV_5(3))
086700180215     C                   EVAL      InSepara = ':'
086800180215     C                   EXSR      SR_SPLIT
086900180215     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(1))
087000180215     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
087100171218
087200180215     C* i dati CCM - LNA - ZNC sono nel secondo segmento divisi dai :
087300180215     C                   EVAL      InStringa =
087400180215     C                                %trim(SkSplitCSV_Sav(2))
087500180215     C* così ottengo una schiera di 3 elementi
087600180215     C                   EVAL      InSepara = ':'
087700180215     C                   EXSR      SR_SPLIT
087800180123     C* siccome sono dati nunmerici controllo che non ci siano valori alfanumerici mediante MONITOR
087900171218     C                   MONITOR
088000180219     C                   EVAL      VABCCM = %dec(%subst(SkSplitCSV_5(1) : 1 : 7)
088100171219     C                                      : 7 : 0)
088200180219     C                   EVAL      VABCTR = %dec(%subst(SkSplitCSV_5(1) : 8 : 3)
088300180219     C                                      : 3 : 0)
088400180124     C                   ON-ERROR
088500180124     C                   Z-ADD     1             wRecErrBlk
088600180124     C                   EVAL      vinmsg = %trimr(vinmsg)
088700180124     C                             + ' ' + 'VABCCM'
088800180124     C                   ENDMON
088900180124     C                   MONITOR
089000180215     C                   EVAL      VABLNA = %dec(SkSplitCSV_5(2)
089100171219     C                                      : 3 : 0)
089200180215     C                   EVAL      VABZNC = %dec(SkSplitCSV_5(3)
089300171219     C                                      : 2 : 0)
089400171218     C                   ON-ERROR
089500171218     C                   EVAL      VABLNA = 0
089600171218     C                   EVAL      VABZNC = 0
089700180124     C                   Z-ADD     1             wRecErrNoBlk
089800171218     C                   ENDMON
089900171031     C                   ENDIF
090000161025     C*
090100161025     C***
090200171108     C* ...tipo record 'GID+' (numero colli)
090300171218     C* E' il numero dei colli della singola delivery in caso di spedizione a colli/ 1 nel caso di
090400171218     C* pallet groupage
090500171108     C                   EVAL      wTag = 'GID+'
090600170209     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090700170209     C                   EVAL      InStringa =
090800180110     C                                %subst(wvindta:%len(%trim(wTag))+1)
090900180110     C* così ottengo una schiera di 2 elementi con 1) nr.progressivo 2) nr. colli+altro da splittare
091000180110     C                   EVAL      InSepara = '+'
091100170209     C                   EXSR      SR_SPLIT
091200180110     C                   EVAL      InStringa =
091300180110     C                                %trim(SkSplitCSV_5(2))
091400180110     C* così ottengo una schiera di 2 elementi con 1) nr.colli 2) tipologia colli
091500180110     C                   EVAL      InSepara = ':'
091600180110     C                   EXSR      SR_SPLIT
091700170209     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
091800170209     C                   EXSR      CHKNUM
091900170209     C                   IF        PiInt=*on
092000171218     C                   Z-ADD     PiVal         VABNCL
092100171218     C***                ADD       PiVal         VABNCL
092200170209     C                   ELSE
092300180124     C                   Z-ADD     1             wRecErrNoBlk
092400180111     C                   EVAL      vinmsg = %trimr(vinmsg)
092500180111     C                             + ' ' + 'VABNCL'
092600170209     C                   ENDIF
092700170209     C                   ENDIF
092800171031     C*
092900171031     C***
093000180111     C* ...tipo record 'MEA+WT++KGM' (peso spedizione)
093100180111     C                   EVAL      wTag = 'MEA+WT++KGM'
093200171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093300171031     C                   EVAL      InStringa =
093400171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
093500171031     C                   EVAL      InSepara = ':'
093600171031     C                   EXSR      SR_SPLIT
093700171108     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
093800171031     C                   EXSR      CHKNUM
093900171031     C                   IF        PiNum=*on
094000180125     C                   EVAL      VABPKB = PiVal/1000
094100171031     C                   ELSE
094200180124     C                   Z-ADD     1             wRecErrNoBlk
094300180111     C                   EVAL      vinmsg = %trimr(vinmsg)
094400180111     C                             + ' ' + 'VABPKB'
094500171031     C                   ENDIF
094600171031     C                   ENDIF
094700171031     C*
094800171031     C***
094900171031     C* ...tipo record 'MEA+VOL++MTQ' (volume spedizione)
095000171108     C***                EVAL      wTag = 'MEA+VOL++MTQ'
095100171108     C                   EVAL      wTag = 'MEA+VOL+AAE+MTQ'
095200171031     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095300171031     C                   EVAL      InStringa =
095400171031     C                                %subst(wvindta:%len(%trim(wTag))+2)
095500171031     C                   EVAL      InSepara = ':'
095600171031     C                   EXSR      SR_SPLIT
095700171108     C                   EVAL      PiStr=%trim(SkSplitCSV_5(1))
095800171031     C                   EXSR      CHKNUM
095900171031     C                   IF        PiNum=*on
096000171218     C                   Z-ADD     PiVal         VABVLB
096100171031     C                   ELSE
096200180124     C                   Z-ADD     1             wRecErrNoBlk
096300180111     C                   EVAL      vinmsg = %trimr(vinmsg)
096400180111     C                             + ' ' + 'VABVLB'
096500171031     C                   ENDIF
096600171031     C                   ENDIF
096700170209     C*
096800170209     C***
096900180111     C* ...tipo record 'RFF+CU' (delivery adidas)
097000180111     C                   EVAL      wTag = 'RFF+CU'
097100171218     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
097200171218     C                   EVAL      wRFF += 1
097300171218     C                   EVAL      SkRFF(wRFF) =
097400171218     C                               %subst(wvindta:%len(%trim(wTag))+2)
097500171218     C                   ENDIF
097600141028     C*
097700141028     C***
097800161013     C* ...tipo record 'GIN+BJ' (dettagli segnacolli)
097900171220     C* NOTA: questo traduttore tratta 2 casi:
098000171220     C* 1 = Sfusi: 1 sped. BRT (segm. UNH), 1 delivery adidas (segm. RFF), ed N coppie HU/segnacolli
098100171220     C*     (segm. GIN).
098200171220     C* 2 = Groupage: 1 sped. BRT (segm. UNH), N delivery adidas (segm. RFF), e 1 coppia
098300171220     C*     HU/segnacolli (segm. GIN).
098400171220     C* Nel segm. GIN ci sono massimo 5 coppie, il primo valore è BRT il secondo è adidas
098500171220     C* impostate così: GIN+BJ+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS+BRT:ADIDAS'
098600171220     C* Quindi OGNI VOLTA che ho determinato un coppia, posso scrivere VAD10 perché:
098700171220     C* - nel caso 1 ho un solo RFF (che ho già recuperato) associato a tutte le coppie
098800171220     C* - nel caso 2 ho N RFF (che ho già recuperato) associati all'unica coppie
098900161013     C                   EVAL      wTag = 'GIN+BJ'
099000141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
099100150526     C                   EVAL      InStringa =
099200150526     C                                %subst(wvindta:%len(%trim(wTag))+2)
099300171220     C                   EVAL      InSepara = '+'
099400171220     C                   EXSR      SR_SPLIT
099500171220     C                   EVAL      SkSplitCSV_Sav = SkSplitCSV_5
099600171220     C                   FOR       wI = 1 to 5
099700171220     C                   IF        %trim(SkSplitCSV_Sav(wI)) <> *blanks
099800171220     C                   EVAL      InStringa =
099900180111     C                             %trim(SkSplitCSV_Sav(wI))
100000171220     C                   EVAL      InSepara = ':'
100100171220     C                   EXSR      SR_SPLIT
100200180115     C                   EVAL      H_VADNCD = %dec(%trim(SkSplitCSV_5(1)):7:0)
100300180115     C                   EVAL      VADNCD = H_VADNCD
100400180115     C                   EVAL      H_VADUN1 = %trim(SkSplitCSV_5(2))
100500171220     C                   EXSR      WRIVAD1
100600171220     C                   ELSE
100700171220     C                   LEAVE
100800150526     C                   ENDIF
100900171220     C                   ENDFOR
101000171108     C                   ENDIF
101100141028     C*
101200141028     C***
101300141028     C* ...tipo record '???' (PDF per packing-list)
101400141028     C                   EVAL      wTag = '???PDF'
101500141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
101600141028     C*
101700141028     C* ......VATNOT_P
101800141028     C                   if        *in53
101900141028     C*
102000141028     C* Solo se valorizzato RMN
102100180221     C                   if        VABRMN <> *zero
102200141028     C                   eval      pIn_MaskPDF =
102300141028     C                                     %trim(%subst(wvindta:1292:100))
102400141028     C                   call      'TIS7P2SER'
102500141028     C                   parm      'W'           pIn_Opz           1
102600141028     C                   parm                    tivlrds
102700141028     C                   parm                    EDIVABDS
102800141028     C                   parm      '10'          pIn_CdIdAz        2
102900141028     C                   parm                    pIn_MaskPDF      50
103000141028     C                   parm      *blanks       pOut_Esito        1
103100141028     C                   endif
103200141028     C                   endif
103300141028     C*
103400141028     C                   ENDIF
103500141028     C*
103600141028     C***
103700141028     C* ...tipo record 'UNT' (fine bolla)
103800141028     C                   EVAL      wTag = 'UNT'
103900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
104000180124     C*
104100180221     C* se c'è stato un errore bloccante
104200180124     C                   IF        wRecErrBlk = 0
104300180124     C*
104400180124     C* se devo scrivere almeno un rcd per VAT
104500180124     C                   IF        wVATNOT_B <> *blank
104600180124     C                   EXSR      WRIVAT
104700180124     C                   ENDIF
104800180124     C*
104900180124     C                   ADD       1             §CTROKVB
105000180124     C                   exsr      CHKWRI
105100180124     C                   WRITE     EDIVAB00
105200150526     C                   ENDIF
105300180124     C*
105400180124     C                   ENDIF
105500010202     C*
105600180124     C* se c'è stato un qualunque errore, lo segnalo sul TIVIN
105700180124     C                   IF        wRecErrBlk = 1 or
105800180124     C                             wRecErrNoBlk = 1
105900180124     C                   ADD       1             §CTRNO
106000180124     C                   EVAL      vinflg = '2'
106100180124     C                   ENDIF
106200000801     C*
106300000801     C                   ENDSR
106400010202     C*----------------------------------------------------*
106500171219     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
106600010202     C*----------------------------------------------------*
106700171219     C     PREVAD1T      BEGSR
106800010202     C*
106900171219     C* Compongo il nome del membro da dare al EDIVATWR
107000010202     C                   eval      parmbr = vlrhdl
107100010202     C                   movel     'M'           parmbr
107200060113     C                   eval      parccm = vlrksc
107300010202     C                   eval      paropz = '1'
107400010202     C* Effettuo la chiamata al CLLE preposto
107500180116     C                   call(e)   'TITVEVD1TC'
107600180116     C***lk              call(e)   'TITVEVDTC'
107700010202     C                   parm                    parccm
107800010202     C                   parm                    parmbr
107900010202     C                   parm                    paropz
108000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
108100010202     C                   if        %error
108200010202     C                   movel     '1'           chkcall
108300010202     C                   else
108400010202     C                   movel     '0'           chkcall
108500010202     C                   endif
108600010202     C*
108700010202     C                   ENDSR
108800141028     C*----------------------------------------------------*
108900141028     C*  CONTROLLO NUMERICITA' CAMPI
109000141028     C*----------------------------------------------------*
109100141028     C     CHKNUM        BEGSR
109200141028     C*
109300141028     C                   IF        PiDecChr = *blanks
109400141028     C                   EVAL      PiDecChr = CharNUM
109500141028     C                   ENDIF
109600141028     C*
109700141028     C                   callp     UBISNUM_Check(PiStr
109800141028     C                                          :PiDecChr
109900141028     C                                          :PiVal
110000141028     C                                          :PiNum
110100141028     C                                          :PiInt)
110200141028     C*
110300141028     C                   ENDSR
110400141028     C***
110500141028
110600141028
110700141028     C*----------------------------------------------------*
110800141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
110900141028     C*----------------------------------------------------*
111000141028     C     SR_SPLIT      BEGSR
111100141028     C*
111200141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
111300141028     C                   CALL      'XSPLIT2'
111400141028     C                   PARM                    InStringa
111500141028     C                   PARM                    InSepara
111600141028     C                   PARM      '5'           InTypeOut
111700141028     C                   PARM      ''            wSkParam
111800141028     C                   PARM                    OutErrore
111900141028     C                   PARM                    OutMsg
112000141028     C                   MOVEA     wSkParam      SkSplitCSV_5
112100141028     C*
112200141028     C                   ENDSR
112300141028
112400141028
112500141028     C*----------------------------------------------------*
112600171220     C*  SCRITTURA BUFFER EDIVAD10F
112700141028     C*----------------------------------------------------*
112800171220     C     WRIVAD1       BEGSR
112900180115     C*
113000180115     C                   EVAL      VADFGS = VABFGS
113100180115     C                   EVAL      VADCCM = VABCCM
113200180115     C                   EVAL      VADLNP = VABLNP
113300180115     C                   EVAL      VADAAS = VABAAS
113400180115     C                   EVAL      VADNRS = VABNRS
113500180115     C                   EVAL      VADNSP = VABNSP
113600180115     C                   EVAL      VADCMR = VABCMR
113700180115     C                   EVAL      VADCNT = VABCNT
113800180116     C                   EVAL      VADNCL = 1
113900180116     C                   ADD       1             §CTROKVD
114000180115     C                   WRITE     EDIVAD00
114100141028     C*
114200180115     C                   IF        H_VADUN1 <> *blanks
114300171220     C                   EVAL      wJ = 0
114400171220     C* devo scrivere tanti rcd quante sono le delivery adidas memorizzate nella schiera SkRFF
114500171220     C* che però è a dimensione variabile
114600171220     C                   DO        *hival
114700171220     C                   EVAL      wJ += 1
114800180116     C                   IF        SkRFF(wJ) = *blank
114900171220     C                   LEAVE
115000171220     C                   ENDIF
115100180115     C*
115200180115     C                   EVAL      H_VADFGS = VABFGS
115300180115     C                   EVAL      H_VADCCM = VABCCM
115400180115     C                   EVAL      H_VADLNP = VABLNP
115500180115     C                   EVAL      H_VADAAS = VABAAS
115600180115     C                   EVAL      H_VADNRS = VABNRS
115700180115     C                   EVAL      H_VADNSP = VABNSP
115800180115     C                   EVAL      H_VADCMR = VABCMR
115900180115     C                   EVAL      H_VADCNT = VABCNT
116000180116     C                   EVAL      H_VADNCL = 1
116100180115     C                   EVAL      H_VADUN2 = SkRFF(wJ)
116200180116     C                   WRITE     EDIVAD100
116300180116     C                   ENDDO
116400171220     C                   ENDIF
116500141028     C*
116600141028     C                   ENDSR
116700000801
116800171219
116900171219     C*----------------------------------------------------*
117000171219     C*  SCRITTURA BUFFER EDIVAT
117100171219     C*----------------------------------------------------*
117200171219     C     WRIVAT        BEGSR
117300171219     C*
117400171219     C                   EVAL      VATFGS = VABFGS
117500171219     C                   EVAL      VATCCM = VABCCM
117600171219     C                   EVAL      VATLNP = VABLNP
117700171219     C                   EVAL      VATAAS = VABAAS
117800171219     C                   EVAL      VATNRS = VABNRS
117900171219     C                   EVAL      VATNSP = VABNSP
118000171219     C                   EVAL      VATCMR = VABCMR
118100171219     C                   EVAL      VATCNT = VABCNT
118200180124     C*
118300180124     C                   if        wVATNOT_B <> *blanks
118400180124     C                   EVAL      VATTRC = 'B'
118500180124     C                   EVAL      VATNOT = wVATNOT_B
118600171219     C                   ADD       1             §CTROKVT
118700171219     C                   WRITE     EDIVAT00
118800171219     C                   ENDIF
118900171219     C*
119000171219     C                   ENDSR
119100171219
119200011113
119300011113     C*----------------------------------------------------*
119400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
119500011113     C*----------------------------------------------------*
119600011113     C     CHKIMPDIV     BEGSR
119700011113     C*
119800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
119900011113     C                   Z-ADD     *zeros        wrkDec            9 9
120000011113     C*
120100011113     C* Come prima cosa effettuo considerazioni sulla divisa
120200011113     C                   IF        vabIAS > *zeros
120300011113     C                   IF        vabVAS <> 'EUR'
120400011113     C                   EVAL      vabVAS =  'ITL'
120500011113     C                   ENDIF
120600011113     C                   ENDIF
120700011113     C*
120800011113     C                   IF        vabCAS > *zeros
120900011113     C                   IF        vabVCA <> 'EUR'
121000011113     C                   EVAL      vabVCA =  'ITL'
121100011113     C                   ENDIF
121200011113     C                   ENDIF
121300011113     C*
121400011113     C                   IF        vabVMD > *zeros
121500020305     C                   IF        vabVAD <> 'EUR'
121600011113     C                   EVAL      vabVAD =  'ITL'
121700011113     C                   ENDIF
121800011113     C                   ENDIF
121900011113     C*
122000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
122100011113     C                   Z-ADD     vabIAS        wrkDec
122200011113     C                   IF        wrkDec > *zeros
122300011113     C                   IF        vabVAS = 'ITL'
122400011113     C                   EVAL      vabIAS = *zeros
122500011113     C                   ENDIF
122600011113     C                   ENDIF
122700011113     C*
122800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
122900011113     C                   Z-ADD     vabCAS        wrkDec
123000011113     C                   IF        wrkDec > *zeros
123100011113     C                   IF        vabVCA = 'ITL'
123200011113     C                   EVAL      vabCAS = *zeros
123300011113     C                   ENDIF
123400011113     C                   ENDIF
123500011113     C*
123600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
123700011113     C                   Z-ADD     vabVMD        wrkDec
123800011113     C                   IF        wrkDec > *zeros
123900011113     C                   IF        vabVAD = 'ITL'
124000011113     C                   EVAL      vabVMD = *zeros
124100011113     C                   ENDIF
124200011113     C                   ENDIF
124300011113     C*
124400011113     C                   ENDSR
124500011113     C***
124600011113
124700011113
124800000801
124900000801
125000990920      /TITLE Invio dei dati al punto operativo.
125100010202     C     invio         BEGSR
125200990920     C*
125300171218     C* 1° invio EDIVAD
125400010201     C                   reset                   dscmz
125500010201     C                   move      vlrpoi        cmzdst
125600180115     C                   eval      cmzfld = 'EDIVADWR'
125700010201     C                   eval      cmzmbd = vlrhdl
125800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
125900021009     C***                if        prmfir = *blanks
126000180115     C                   eval      cmzfla = 'EDIVAD0F'
126100180115     C                   eval      cmzmba = 'EDIVAD0F'
126200021009     C***                else
126300021009     C***                eval      cmzfla = prmfir
126400021009     C***                eval      cmzmba = prmfir
126500021009     C***                endif
126600010201     C                   eval      cmznrr = *zeros
126700180116     C                   move      §ctrokvd      cmznrr
126800021018     C                   eval      cmzlba = vlrfl1
126900180116     C                   call(e)   'TIS711C'
127000180116     C                   parm                    dscmz
127100180116     C                   parm      *blanks       esito
127200010205     C                   if        %error
127300010205     C                             or cmzerr = '1'
127400010205     C                             or esito  = '1'
127500010205     C                   eval      wrkesito = '3'
127600010205     C                   else
127700180116     C*
127800180116     C* 1°bis invio EDIVAD10
127900180116     C                   reset                   dscmz
128000180116     C                   move      vlrpoi        cmzdst
128100180116     C                   eval      cmzfld = 'EDIVAD1WR'
128200180116     C                   eval      cmzmbd = vlrhdl
128300180116     C                   eval      %subst(cmzmbd:1:1) = 'M'
128400180116     C***                if        prmfir = *blanks
128500180116     C                   eval      cmzfla = 'EDIVAD10F'
128600180116     C                   eval      cmzmba = 'EDIVAD10F'
128700180116     C***                else
128800180116     C***                eval      cmzfla = prmfir
128900180116     C***                eval      cmzmba = prmfir
129000180116     C***                endif
129100180116     C                   eval      cmznrr = *zeros
129200180116     C                   move      §ctrokvd      cmznrr
129300180116     C                   eval      cmzlba = vlrfl1
129400180116     C                   call(e)   'TIS711C'
129500180116     C                   parm                    dscmz
129600180116     C                   parm      *blanks       esito
129700180116     C                   if        %error
129800180116     C                             or cmzerr = '1'
129900180116     C                             or esito  = '1'
130000180116     C                   eval      wrkesito = '3'
130100180116     C                   else
130200171219     C*
130300171219     C* 2° invio EDIVAT
130400171219     C                   reset                   dscmz
130500171219     C                   move      vlrpoi        cmzdst
130600171219     C                   eval      cmzfld = 'EDIVATWR'
130700171219     C                   eval      cmzmbd = vlrhdl
130800171219     C                   eval      %subst(cmzmbd:1:1) = 'M'
130900171219     C***                if        prmfir = *blanks
131000171219     C                   eval      cmzfla = 'EDIVAT0F'
131100171219     C                   eval      cmzmba = 'EDIVAT0F'
131200171219     C***                else
131300171219     C***                eval      cmzfla = prmfir
131400171219     C***                eval      cmzmba = prmfir
131500171219     C***                endif
131600171219     C                   eval      cmznrr = *zeros
131700171219     C                   move      §ctrokvt      cmznrr
131800171219     C                   eval      cmzlba = vlrfl1
131900171219     C                   call(e)   'TIS711C'
132000171219     C                   parm                    dscmz
132100171219     C                   parm      *blanks       esito
132200171219     C                   if        %error
132300171219     C                             or cmzerr = '1'
132400171219     C                             or esito  = '1'
132500171219     C                   eval      wrkesito = '3'
132600171219     C                   else
132700010201     C*
132800171219     C* 3° invio EDIVAB
132900010201     C                   reset                   dscmz
133000010201     C                   move      vlrpoi        cmzdst
133100010201     C                   eval      cmzfld = vlrfou
133200010201     C                   eval      cmzmbd = vlrhdl
133300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
133400021009     C***                if        prmfir = *blanks
133500140117     C                   eval      cmzfla = 'EDIVAB0F'
133600140117     C                   eval      cmzmba = 'EDIVAB0F'
133700021009     C***                else
133800021009     C***                eval      cmzfla = prmfir
133900021009     C***                eval      cmzmba = prmfir
134000021009     C***                endif
134100010201     C                   eval      cmznrr = *zeros
134200010201     C                   move      §ctrokvb      cmznrr
134300021018     C                   eval      cmzlba = vlrfl1
134400010201     C                   call(e)   'TIS711C'
134500010201     C                   parm                    dscmz
134600010201     C                   parm      *blanks       esito
134700010201     C                   if        %error
134800010201     C                             or cmzerr = '1'
134900010201     C                             or esito  = '1'
135000010201     C                   eval      wrkesito = '3'
135100010201     C                   endif
135200180116     C                   endif
135300010205     C                   endif
135400171219     C                   endif
135500990920     C*
135600000613     C                   ENDSR
135700000613     C***
135800070411
135900141028
136000070411     C     *pssr         BEGSR
136100070411     C*
136200070411     C                   if        %open(tivin00r)
136300070411     C                   close     tivin00r
136400070411     C                   endif
136500140117     C                   if        %open(edivabwr)
136600140117     C                   close     edivabwr
136700070411     C                   endif
136800180116     C                   if        %open(EDIVAD1wr)
136900180116     C                   close     EDIVAD1wr
137000180116     C                   endif
137100180115     C                   if        %open(EDIVADwr)
137200180115     C                   close     EDIVADwr
137300171219     C                   endif
137400180115     C                   if        %open(EDIVATwr)
137500180115     C                   close     EDIVATwr
137600180115     C                   endif
137700070411     C*
137800070411     C* Effettuo la chiamata al CLLE preposto
137900180116     C                   call(e)   'TITVEVD1TC'
138000180116     C***lk              call(e)   'TITVEVDTC'
138100070411     C                   parm                    parccm
138200070411     C                   parm                    parmbr
138300070411     C                   parm      '2'           paropz
138400070411     C*
138500070411     C                   eval      wrkesito = '2'
138600070411     C*
138700070411     C                   seton                                        LR
138800070411     C*
138900070411     C                   ENDSR     '*CANCL'
139000070411     C***
139100161018
139200161018
139300161018     C*--------------------------------------------------------
139400161018     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
139500161018     C*--------------------------------------------------------
139600161018     C     CARTAB        BEGSR
139700161018     C*
139800161018     C* TABELLA '15' - NAZIONI
139900161018     C                   clear                   skNAZISO
140000161018     C                   clear                   skNAZBAR
140100161018     C                   eval      tblKUT = 1
140200161018     C                   eval      tblCOD = '15'
140300161018     C     KEYtabP       setll     tabel00f
140400161018     C     KEYtabP       reade     tabel00f
140500161018     C                   dow       not %eof(tabel00f)
140600161018     C                   if        tblFLG = *blanks
140700161018     C                   movel(p)  tblUNI        ds15
140800161018     C                   add       1             jNAZ
140900161018     C                   eval      skNAZBAR(jNAZ) = tblKEY
141000161018     C                   eval      skNAZISO(jNAZ) = §15COD
141100161018     C                   endif
141200161018     C     KEYtabP       reade     tabel00f
141300161018     C                   enddo
141400161018     C*
141500161018     C                   ENDSR
141600161018     C***
141700070411
141800990910
141900000613     C     *inzsr        BEGSR
142000990910     C*
142100990910     C     *entry        plist
142200990920     C                   parm                    tivlrds
142300990921     C                   parm      wrkesito      esito
142400000724     C                   parm                    prmlit
142500000710     C                   parm                    prmfir
142600000613     C*
142700000830     C* CALCOLA LA DATA CORRENTE
142800140117     C                   time                    wn14             14 0
142900140117     C                   movel     wn14          oracor            6 0          *ORA
143000100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
143100100722     C                   eval      datcor = %dec(%date() : *ISO)
143200161018     C*
143300161018     C* Chiave su TABEL00F - parziale
143400161018     C     KEYtabP       KLIST
143500161018     C                   KFLD                    tblKUT
143600161018     C                   KFLD                    tblCOD
143700000830     C*
143800000613     C                   ENDSR
143900000613     C***
