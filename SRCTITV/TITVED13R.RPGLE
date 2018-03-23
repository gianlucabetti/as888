000100140117      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400160801     Ftabel00f  if   e           k disk
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
001600160801     D ds15          e ds
001700070719     D tisi95ds      e ds
001800140117     D edivabds      e ds                  extname(edivab0f)
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700141028     D wTag            s            128    VARYING INZ
002800141028     D wRecErr         s              1  0 INZ(*zeros)
002900141028     D wFlgCAS         s              1    INZ(*blanks)
003000141028     D wVINDTA         s                   LIKE(vinDTA) INZ
003100141028     D wVATNOT_A       s                   LIKE(VATNOT) INZ
003200141028     D wVATNOT_B       s                   LIKE(VATNOT) INZ
003300141028     D wVATNOT_E       s                   LIKE(VATNOT) INZ
003400141028     D wVATNOT_I       s                   LIKE(VATNOT) INZ
003500141028     D wVATNOT_J       s                   LIKE(VATNOT) INZ
003600141028     D wVATNOT_S       s                   LIKE(VATNOT) INZ
003700141028     D wVATNOT_P       s                   LIKE(VATNOT) INZ
003800160606
003900160801     D*------------
004000160801     D jNAZ            s              5  0 INZ(*zeros)
004100160801     D skNAZISO        S              3    DIM(1000)
004200160801     D skNAZBAR        S              3    DIM(1000)
004300160606
004400160606     D*------------------
004500160606     D* DS RIDEFINIZIONE SEGNACOLLO BRT
004600160606     D*------------------
004700160606     D SEGNBRT         ds                  inz qualified
004800160620     D  LNP                           3  0
004900160606     D  LNA                           3  0
005000160606     D  NRS                           2  0
005100160606     D  NCD                           7  0
005200160606     D  ZNC                           2  0
005300160606     D  CHKDGT                        1
005400160606
005500141028
005600141028     D*------------------
005700141028     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005800141028     D*------------------
005900141028     D CharNUM         S              1
006000141028     D CharSOS         S              1
006100141028     D posDaDft        S              3  0 INZ(*zeros)
006200141028     D posADft         S              3  0 INZ(*zeros)
006300141028     D wGiroDft        s              1  0 INZ(*zeros)
006400160621
006500160621
006600160621     D*-----------
006700160621     D* VARIABILI X LA VERIFICA CORRETTEZZA FORMALE SEGNACOLLO BRT (TRAMITE DRIVER UBBRTSEGR)
006800160621     D*-----------
006900160621     D pInBRT_SEGN     S             18A
007000161018     D pInLNP01        S              3S 0
007100161018     D pInLNP02        S              3S 0
007200161018     D pInLNP03        S              3S 0
007300161018     D pInLNP04        S              3S 0
007400161018     D pInLNP05        S              3S 0
007500161018     D pInNRS01        S              2S 0
007600161018     D pInNRS02        S              2S 0
007700161018     D pInNRS03        S              2S 0
007800161018     D pInNRS04        S              2S 0
007900161018     D pInNRS05        S              2S 0
008000161018     D pInNRS06        S              2S 0
008100161018     D pInNRS07        S              2S 0
008200161018     D pInNRS08        S              2S 0
008300161018     D pInNRS09        S              2S 0
008400161018     D pInNRS10        S              2S 0
008500170928     D pInNRS11        S              2S 0
008600170928     D pInNRS12        S              2S 0
008700170928     D pInNRS13        S              2S 0
008800170928     D pInNRS14        S              2S 0
008900170928     D pInNRS15        S              2S 0
009000170928     D pInNRS16        S              2S 0
009100170928     D pInNRS17        S              2S 0
009200170928     D pInNRS18        S              2S 0
009300170928     D pInNRS19        S              2S 0
009400170928     D pInNRS20        S              2S 0
009500160621     D pInChkFil       S              1A
009600160621     D pInChkDgt       S              1A
009700160621     D pOutEsitoOK     S              1N
009800141028
009900141028
010000141028     D*------------------
010100141028     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
010200141028     D*------------------
010300141028     D InStringa       S          65535A   VARYING                              (stringa input)
010400141028     D InSepara        S             10A                                        (separatore)
010500141028     D InTypeOut       S              1A                                        (tipo output)
010600141028     D wSkParam        S          65535A                                        (output)
010700141028     D OutErrore       S              1A                                        (flag errore)
010800141028     D OutMsg          S             80A                                        (messaggio errore)
010900141028     D SkSplitCSV_5    S            256    DIM(256)
011000141028
011100000830
011200041025     D*------------------
011300041025     D* DS REPERIMENTO NUMERATORE
011400041025     D*------------------
011500041025     D trul33ds      e ds                  inz
011600041025     D*------------------
011700041025     D* DS ARCHITETTURA
011800041025     D*------------------
011900041025     D kpjba         e ds                  inz
012000041025     D*------------------
012100990908
012200141028
012300141028     D*------------------
012400141028     D* LINKING A DEFINIZIONI ESTERNE
012500141028     D*------------------
012600141028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
012700141028     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
012800141028
012900141028
013000010201
013100010201
013200990921     C                   reset                   esito
013300990921     C                   reset                   wrkesito
013400000613     C*
013500160801     C                   EXSR      CARTAB                                       CARICA TABELLE
013600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
013700070719     C*
013800070719     C* Esegue lancio TISI95R solo x chiusura
013900070719     C                   CLEAR                   TISI95DS
014000070719     C                   EVAL      I95TLA = 'C'
014100070719     C                   CALL      'TISI95R'
014200070719     C                   PARM                    TISI95DS
014300000613     C*
014400010202     C* Effettuo la chiamata al CLLE preposto
014500140922     C                   call(e)   'TITVEVTC'
014600140922     C                   parm                    parccm
014700140922     C                   parm                    parmbr
014800140922     C                   parm      '2'           paropz
014900170731     C*
015000170731     C                   if        *in53
015100170731     C                   call      'TIS7P2SER'
015200170731     C                   parm      'C'           pIn_Opz           1
015300170731     C                   parm                    tivlrds
015400170731     C                   parm                    EDIVABDS
015500170731     C                   parm      *blanks       pIn_CdIdAz        2
015600170731     C                   parm      *blanks       pIn_MaskPDF      50
015700170731     C                   parm      *blanks       pOut_Esito        1
015800170731     C                   endif
015900000616     C*
016000010201     C                   seton                                        LR
016100990908
016200000801
016300910830     C*--------------------------------------------------------
016400140117     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
016500910830     C*--------------------------------------------------------
016600040526     C     RWFILE        BEGSR
016700990910     C*
016800990914     C                   if        not %open(tivin00r)
016900990908     C                   open      tivin00r
017000990914     C                   endif
017100140117     C                   if        not %open(edivabwr)
017200140117     C                   open      edivabwr
017300990914     C                   endif
017400141028     C*
017500140117     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
017600020305     C                   exsr      prevat
017700010201     C*
017800010202     C                   if        chkcall = '0'
017900010202     C*
018000140117     C                   if        not %open(edivatwr)
018100140117     C                   open      edivatwr
018200010201     C                   endif
018300990910     C*
018400010201     C                   clear                   §CTROKVB          5 0
018500020305     C                   clear                   §CTROKVT          5 0
018600000801     C                   clear                   §CTRMO            5 0
018700000801     C                   clear                   §CTRNO            5 0
018800141028     C*
018900141028     C* Inizializzazioni fuori ciclo
019000150127     C                   SETOFF                                       40
019100141028     C                   EXSR      INZVAR
019200140117     C*
019300140902     C                   if        *in53
019400140117     C                   call      'TIS7P2SER'
019500140117     C                   parm      'O'           pIn_Opz           1
019600140117     C                   parm                    tivlrds
019700140117     C                   parm                    EDIVABDS
019800140117     C                   parm      *blanks       pIn_CdIdAz        2
019900140117     C                   parm      *blanks       pIn_MaskPDF      50
020000140117     C                   parm      *blanks       pOut_Esito        1
020100140902     C                   endif
020200150127     C*
020300921023     C                   DO        *HIVAL
020400990913     C*
020500990915     C                   READ      tivin00r                               70
020600040910     C                   if        vindta > *blanks
020700000801     C*
020800000801     C                   if        *in70 = *off
020900000801     C                             and
021000000801     C                             (vinflg = *blanks
021100000801     C                              or vinflg = '0'
021200000801     C                              or vinflg = '2')
021300000801     C*
021400000801     C                   clear                   vinmsg
021500141028     C*
021600141028     C* "normalizzo" il dato di input
021700141028     C                   eval      wVINDTA = %trim(%subst(vindta:1:
021800141028     C                                             %len(%trim(vindta))-1))
021900160209     C*
022000160209     C* Elimino le "duple di controllo EDIFACT"
022100160209     C                   eval      wVINDTA = %scanrpl('?''':' ':wVINDTA)
022200160209     C                   eval      wVINDTA = %scanrpl('?+':' ':wVINDTA)
022300160209     C                   eval      wVINDTA = %scanrpl('?:':' ':wVINDTA)
022400160209     C                   eval      wVINDTA = %scanrpl('??':' ':wVINDTA)
022500160209     C                   eval      wVINDTA = %scanrpl('?.':' ':wVINDTA)
022600040910     C*
022700040910     C* Eseguo routine d traduzione
022800040910     C                   exsr      impvabvat
022900040802     C*
023000010305     C                   endif
023100000905     C*
023200000905     C                   else
023300000905     C                   eval      vinflg = '1'
023400000905     C                   endif
023500000905     C*
023600000905     C  N70              update    tivin000
023700000905     C*
023800991022     C  N70              ENDdo
023900100722     C*
024000100722     C* Scarico i buffer testata ancora "in canna"
024100140922     C*
024200141028     C***  N31              ADD       1             §CTROKVB
024300141028     C***  N31              exsr      CHKWRI
024400141028     C***  N31              WRITE     EDIVAB00
024500010202     C*
024600010202     C                   endif
024700990910
024800990910     C* Se non ci sono record con errori ...
024900000710     C                   if        §ctrno = 0
025000990910     C* ... restituisco esito OK.
025100990921     C                   eval      wrkesito = '0'
025200990910     C                   else
025300010201     C                   if        §ctrokvb > 0
025400990921     C                   eval      wrkesito = '1'
025500000710     C                   else
025600000710     C                   eval      wrkesito = '2'
025700990910     C                   endif
025800000710     C                   endif
025900990910     C*
026000990914     C                   if        %open(tivin00r)
026100990908     C                   close     tivin00r
026200990914     C                   endif
026300140117     C                   if        %open(edivabwr)
026400140117     C                   close     edivabwr
026500990914     C                   endif
026600140117     C                   if        %open(edivatwr)
026700140117     C                   close     edivatwr
026800010201     C                   endif
026900990910     C*
027000010201     C                   if        §ctrokvb > 0
027100000724     C                             and vlrpoi <> *zeros
027200010202     C                   exsr      invio
027300990920     C                   endif
027400990920     C*
027500910830     C                   ENDSR
027600000613     C***
027700140922
027800140922     C*----------------------------------------------------*
027900140922     C*  FORZATURE E CONSIDERAZIONI PRE-WRITE TESTATA
028000140922     C*----------------------------------------------------*
028100140922     C     CHKWRI        BEGSR
028200140922     C*
028300141028     C                   if        VABRMN = *zeros
028400141028     C                   eval      VABRMN = VABNSP
028500141028     C                   endif
028600141028     C*
028700150127     C                   if        VABNCL = *zeros
028800150127     C                   eval      VABNCL = cntVABNCL
028900150127     C                   endif
029000150127     C*
029100141028     C                   if        VABPRD = *blanks
029200141028     C* ......VABPRD
029300141028     C* Reperisco la provincia dal CAP e dalla località
029400141028     C                   IF        VABLOD <> *blanks AND
029500141028     C                             VABCAD <> *blanks AND
029600141028     C                             VABNZD  = *blanks
029700141028     C                   CLEAR                   TISI95DS
029800141028     C                   EVAL      I95TCN = '3'
029900141028     C                   Z-ADD     datcor        I95DAT
030000141028     C                   EVAL      I95CAP = VABCAD
030100141028     C                   EVAL      I95LOC = VABLOD
030200141028     C                   CALL      'TISI95R'
030300141028     C                   PARM                    TISI95DS
030400141028     C                   EVAL      VABPRD = O95PRV
030500141028     C                   ENDIF
030600141028     C                   endif
030700140922     C*
030800140922     C* Considerazioni sul contenuto di campi precedentemente valorizzati
030900141028     C                   IF        wFlgCAS <> '0'
031000140922     C                   IF        VABCBO = '1'
031100140922     C                   EVAL      VABCBO = '4'
031200140922     C                   ELSE
031300140922     C                   EVAL      VABCBO = '6'
031400140922     C                   ENDIF
031500140922     C                   ENDIF
031600140922     C*
031700140922     C* Eseguo routine finale x considerazioni specifiche su importi/divise
031800141028     C                   EXSR      CHKIMPDIV
031900140922     C*
032000140922     C                   ENDSR
032100141028     C*----------------------------------------------------*
032200141028     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032300141028     C*----------------------------------------------------*
032400141028     C     INZVAR        BEGSR
032500141028     C*
032600141028     C* Inizializzo variabili di wrk
032700141028     C                   Z-ADD     *zeros        Num5_0            5 0
032800150127     C                   Z-ADD     *zeros        cntVABNCL         5 0
032900141028     C                   MOVEL     '0'           wFlgCAS
033000141028     C                   MOVEL     *blanks       wVATNOT_A
033100141028     C                   MOVEL     *blanks       wVATNOT_B
033200141028     C                   MOVEL     *blanks       wVATNOT_E
033300141028     C                   MOVEL     *blanks       wVATNOT_I
033400141028     C                   MOVEL     *blanks       wVATNOT_J
033500141028     C                   MOVEL     *blanks       wVATNOT_S
033600141028     C                   MOVEL     *blanks       wVATNOT_P
033700141028     C                   CLEAR                   SkSplitCSV_5
033800141028     C*
033900141028     C* Reimposto i valori di default
034000141028     C                   EXSR      DEFCAM
034100141028     C*
034200141028     C                   ENDSR
034300141028     C*----------------------------------------------------*
034400141028     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
034500141028     C*----------------------------------------------------*
034600141028     C     DEFCAM        BEGSR
034700141028     C*
034800141028     C                   CLEAR                   EDIVAB00
034900141028     C                   CLEAR                   EDIVAT00
035000141028     C*
035100141028     C* Imposto i valori bolla di default
035200141028     C                   EVAL      VABCTR = 000
035300141028     C                   EVAL      VABCBO = '1'
035400150127     C                   EVAL      VABTSP = 'C'
035500141028     C*
035600141028     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035700141028     C* e delimitatore testo.
035800141028     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
035900141028     C*
036000141028     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
036100141028     C                   EVAL      CharSOS = CharNUM
036200141028     C*
036300141028     C* Reperisco i flag che indicano comportamenti particolari del traduttore
036400160623     C*
036500160623     C* - Gestione packing-list PDF
036600141028     C                   SETOFF                                       53
036700141028     C                   SELECT
036800160623     C                   WHEN      %subst(vlrppt:2:2) = 'PF'
036900141028     C                   SETON                                        53
037000141028     C                   ENDSL
037100160623     C*
037200160623     C* - Gestione forzatura tutto Disk C
037300160623     C                   SETOFF                                       55
037400160623     C                   SELECT
037500160623     C                   WHEN      %subst(vlrppt:1:1) = 'C'
037600160623     C                   SETON                                        55
037700160623     C                   ENDSL
037800141028     C*
037900141028     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038000141028     C                   EVAL      posDaDft = 1
038100141028     C                   EVAL      posADft  = 0
038200141028     C                   EVAL      wGiroDft = 0
038300141028     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
038400141028     C                             posDaDft > 0
038500141028     C*
038600141028     C* Gestisco il 1° giro
038700141028     C                   IF        wGiroDft = 0
038800141028     C* Eseguo lo scan x trovare l'inizio del campo corrente
038900141028     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039000141028     C* Incremento il contatore dei "giri"
039100141028     C                   EVAL      wGiroDft = 1
039200141028     C                   ELSE
039300141028     C                   EVAL      posDaDft = posADft
039400141028     C                   ENDIF
039500141028     C* Eseguo lo scan x trovare la fine del campo corrente
039600141028     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
039700141028     C*
039800141028     C* A questo "estraggo" il parametro (campo e valore) corrente...
039900141028     C* ...solo se entrambe le posizini (DA/A) sono > 0
040000141028     C                   IF        posDaDft > 0 AND
040100141028     C                             posADft  > 0
040200141028     C* NCL
040300141028     C                   IF        %subst(
040400141028     C                             %subst(vlrppt:posDaDft+1:
040500141028     C                             posADft-posDaDft-1):1:3)
040600141028     C                             = 'NCL'
040700141028     C                   EVAL      PiStr=%trim(%subst(
040800141028     C                             %subst(vlrppt:posDaDft+1:
040900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041000141028     C                   EXSR      CHKNUM
041100141028     C                   IF        PiInt=*on
041200141028     C                   Z-ADD     PiVal         VABNCL
041300141028     C                   ENDIF
041400141028     C                   ENDIF
041500141028     C* CCM
041600141028     C                   IF        %subst(
041700141028     C                             %subst(vlrppt:posDaDft+1:
041800141028     C                             posADft-posDaDft-1):1:3)
041900141028     C                             = 'CCM'
042000141028     C                   EVAL      PiStr=%trim(%subst(
042100141028     C                             %subst(vlrppt:posDaDft+1:
042200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042300141028     C                   EXSR      CHKNUM
042400141028     C                   IF        PiInt=*on
042500141028     C                   Z-ADD     PiVal         VABCCM
042600141028     C                   ENDIF
042700141028     C                   ENDIF
042800141028     C* LNP
042900141028     C                   IF        %subst(
043000141028     C                             %subst(vlrppt:posDaDft+1:
043100141028     C                             posADft-posDaDft-1):1:3)
043200141028     C                             = 'LNP'
043300141028     C                   EVAL      PiStr=%trim(%subst(
043400141028     C                             %subst(vlrppt:posDaDft+1:
043500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043600141028     C                   EXSR      CHKNUM
043700141028     C                   IF        PiInt=*on
043800141028     C                   Z-ADD     PiVal         VABLNP
043900141028     C                   ENDIF
044000141028     C                   ENDIF
044100141028     C* NRS
044200141028     C                   IF        %subst(
044300141028     C                             %subst(vlrppt:posDaDft+1:
044400141028     C                             posADft-posDaDft-1):1:3)
044500141028     C                             = 'NRS'
044600141028     C                   EVAL      PiStr=%trim(%subst(
044700141028     C                             %subst(vlrppt:posDaDft+1:
044800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044900141028     C                   EXSR      CHKNUM
045000141028     C                   IF        PiInt=*on
045100141028     C                   Z-ADD     PiVal         VABNRS
045200141028     C                   ENDIF
045300141028     C                   ENDIF
045400141028     C* CTR
045500141028     C                   IF        %subst(
045600141028     C                             %subst(vlrppt:posDaDft+1:
045700141028     C                             posADft-posDaDft-1):1:3)
045800141028     C                             = 'CTR'
045900141028     C                   EVAL      PiStr=%trim(%subst(
046000141028     C                             %subst(vlrppt:posDaDft+1:
046100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046200141028     C                   EXSR      CHKNUM
046300141028     C                   IF        PiInt=*on
046400141028     C                   Z-ADD     PiVal         VABCTR
046500141028     C                   ENDIF
046600141028     C                   ENDIF
046700141028     C* PKB
046800141028     C                   IF        %subst(
046900141028     C                             %subst(vlrppt:posDaDft+1:
047000141028     C                             posADft-posDaDft-1):1:3)
047100141028     C                             = 'PKB'
047200141028     C                   EVAL      PiStr=%trim(%subst(
047300141028     C                             %subst(vlrppt:posDaDft+1:
047400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047500141028     C                   EXSR      CHKNUM
047600141028     C                   IF        PiNum=*on
047700141028     C                   Z-ADD     PiVal         VABPKB
047800141028     C                   ENDIF
047900141028     C                   ENDIF
048000141028     C* VLB
048100141028     C                   IF        %subst(
048200141028     C                             %subst(vlrppt:posDaDft+1:
048300141028     C                             posADft-posDaDft-1):1:3)
048400141028     C                             = 'VLB'
048500141028     C                   EVAL      PiStr=%trim(%subst(
048600141028     C                             %subst(vlrppt:posDaDft+1:
048700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800141028     C                   EXSR      CHKNUM
048900141028     C                   IF        PiNum=*on
049000141028     C                   Z-ADD     PiVal         VABVLB
049100141028     C                   ENDIF
049200141028     C                   ENDIF
049300141028     C* QFT
049400141028     C                   IF        %subst(
049500141028     C                             %subst(vlrppt:posDaDft+1:
049600141028     C                             posADft-posDaDft-1):1:3)
049700141028     C                             = 'QFT'
049800141028     C                   EVAL      PiStr=%trim(%subst(
049900141028     C                             %subst(vlrppt:posDaDft+1:
050000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100141028     C                   EXSR      CHKNUM
050200141028     C                   IF        PiNum=*on
050300141028     C                   Z-ADD     PiVal         VABQFT
050400141028     C                   ENDIF
050500141028     C                   ENDIF
050600141028     C* CBO
050700141028     C                   IF        %subst(
050800141028     C                             %subst(vlrppt:posDaDft+1:
050900141028     C                             posADft-posDaDft-1):1:3)
051000141028     C                             = 'CBO'
051100141028     C                   EVAL      VABCBO=%trim(%subst(
051200141028     C                             %subst(vlrppt:posDaDft+1:
051300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400141028     C                   ENDIF
051500141028     C* TSP
051600141028     C                   IF        %subst(
051700141028     C                             %subst(vlrppt:posDaDft+1:
051800141028     C                             posADft-posDaDft-1):1:3)
051900141028     C                             = 'TSP'
052000141028     C                   EVAL      VABTSP=%trim(%subst(
052100141028     C                             %subst(vlrppt:posDaDft+1:
052200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300141028     C                   ENDIF
052400141028     C* VAS
052500141028     C                   IF        %subst(
052600141028     C                             %subst(vlrppt:posDaDft+1:
052700141028     C                             posADft-posDaDft-1):1:3)
052800141028     C                             = 'VAS'
052900141028     C                   EVAL      VABVAS=%trim(%subst(
053000141028     C                             %subst(vlrppt:posDaDft+1:
053100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200141028     C                   ENDIF
053300141028     C* VCA
053400141028     C                   IF        %subst(
053500141028     C                             %subst(vlrppt:posDaDft+1:
053600141028     C                             posADft-posDaDft-1):1:3)
053700141028     C                             = 'VCA'
053800141028     C                   EVAL      VABVCA=%trim(%subst(
053900141028     C                             %subst(vlrppt:posDaDft+1:
054000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100141028     C                   ENDIF
054200141028     C* TIC
054300141028     C                   IF        %subst(
054400141028     C                             %subst(vlrppt:posDaDft+1:
054500141028     C                             posADft-posDaDft-1):1:3)
054600141028     C                             = 'TIC'
054700141028     C                   EVAL      VABTIC=%trim(%subst(
054800141028     C                             %subst(vlrppt:posDaDft+1:
054900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000141028     C                   ENDIF
055100141028     C* GCA
055200141028     C                   IF        %subst(
055300141028     C                             %subst(vlrppt:posDaDft+1:
055400141028     C                             posADft-posDaDft-1):1:3)
055500141028     C                             = 'GCA'
055600141028     C                   EVAL      VABGCA=%trim(%subst(
055700141028     C                             %subst(vlrppt:posDaDft+1:
055800141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900141028     C                   ENDIF
056000141028     C* CTM
056100141028     C                   IF        %subst(
056200141028     C                             %subst(vlrppt:posDaDft+1:
056300141028     C                             posADft-posDaDft-1):1:3)
056400141028     C                             = 'CTM'
056500141028     C                   EVAL      VABCTM=%trim(%subst(
056600141028     C                             %subst(vlrppt:posDaDft+1:
056700141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800141028     C                   ENDIF
056900141028     C* FFD
057000141028     C                   IF        %subst(
057100141028     C                             %subst(vlrppt:posDaDft+1:
057200141028     C                             posADft-posDaDft-1):1:3)
057300141028     C                             = 'FFD'
057400141028     C                   EVAL      VABFFD=%trim(%subst(
057500141028     C                             %subst(vlrppt:posDaDft+1:
057600141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700141028     C                   ENDIF
057800141028     C* VAD
057900141028     C                   IF        %subst(
058000141028     C                             %subst(vlrppt:posDaDft+1:
058100141028     C                             posADft-posDaDft-1):1:3)
058200141028     C                             = 'VAD'
058300141028     C                   EVAL      VABVAD=%trim(%subst(
058400141028     C                             %subst(vlrppt:posDaDft+1:
058500141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600141028     C                   ENDIF
058700141028     C* GMA
058800141028     C                   IF        %subst(
058900141028     C                             %subst(vlrppt:posDaDft+1:
059000141028     C                             posADft-posDaDft-1):1:3)
059100141028     C                             = 'GMA'
059200141028     C                   EVAL      VABGMA=%trim(%subst(
059300141028     C                             %subst(vlrppt:posDaDft+1:
059400141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500141028     C                   ENDIF
059600141028     C* GGA
059700141028     C                   IF        %subst(
059800141028     C                             %subst(vlrppt:posDaDft+1:
059900141028     C                             posADft-posDaDft-1):1:3)
060000141028     C                             = 'GGA'
060100141028     C                   EVAL      VABGGA=%trim(%subst(
060200141028     C                             %subst(vlrppt:posDaDft+1:
060300141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400141028     C                   ENDIF
060500141028     C* GVA
060600141028     C                   IF        %subst(
060700141028     C                             %subst(vlrppt:posDaDft+1:
060800141028     C                             posADft-posDaDft-1):1:3)
060900141028     C                             = 'GVA'
061000141028     C                   EVAL      VABGVA=%trim(%subst(
061100141028     C                             %subst(vlrppt:posDaDft+1:
061200141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300141028     C                   ENDIF
061400141028     C* TC1
061500141028     C                   IF        %subst(
061600141028     C                             %subst(vlrppt:posDaDft+1:
061700141028     C                             posADft-posDaDft-1):1:3)
061800141028     C                             = 'TC1'
061900141028     C                   EVAL      VABTC1=%trim(%subst(
062000141028     C                             %subst(vlrppt:posDaDft+1:
062100141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200141028     C                   ENDIF
062300141028     C* TC2
062400141028     C                   IF        %subst(
062500141028     C                             %subst(vlrppt:posDaDft+1:
062600141028     C                             posADft-posDaDft-1):1:3)
062700141028     C                             = 'TC2'
062800141028     C                   EVAL      VABTC2=%trim(%subst(
062900141028     C                             %subst(vlrppt:posDaDft+1:
063000141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100141028     C                   ENDIF
063200141028     C* VATTRC
063300141028     C                   IF        %subst(
063400141028     C                             %subst(vlrppt:posDaDft+1:
063500141028     C                             posADft-posDaDft-1):1:3)
063600141028     C                             = 'TRC'
063700141028     C                   EVAL      VATTRC=%trim(%subst(
063800141028     C                             %subst(vlrppt:posDaDft+1:
063900141028     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000141028     C                   ENDIF
064100141028     C* ...
064200141028     C                   ENDIF
064300141028     C                   ENDDO
064400141028     C*
064500141028     C                   ENDSR
064600000801     C*----------------------------------------------------*
064700140117     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
064800000801     C*----------------------------------------------------*
064900040910     C     IMPVABVAT     BEGSR
065000040910     C*
065100040910     C* Traduzione relativa ai tipi record del file del cliente
065200040910     C*
065300071210     C*
065400071210     C***
065500141028     C* ...tipo record 'UNB' (info CMR)
065600141028     C                   EVAL      wTag = 'UNB'
065700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
065800141028     C                   MOVEL     *blanks       savCMR           35
065900141028     C                   EVAL      InStringa =
066000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
066100141028     C                   EVAL      InSepara = '+'
066200141028     C                   EXSR      SR_SPLIT
066300141028     C                   EVAL      savCMR = %trim(SkSplitCSV_5(5)) + ' '+
066400140922     C                                      %char(datcor) + ' ' + %char(oracor)
066500130620     C                   ENDIF
066600150127     C*
066700150127     C***
066800150127     C* ...tipo record 'NAD+SF' (info testata Mittente Originale)
066900160929     C***                EVAL      wTag = 'NAD+SF'
067000160929     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067100150127     C                   MOVEL     *blanks       savRMO           25
067200150127     C                   MOVEL     *blanks       savCMO            9
067300150127     C                   MOVEL     *blanks       savNMO            3
067400160929     C***                EVAL      InStringa =
067500160929     C***                             %subst(wvindta:%len(%trim(wTag))+2)
067600160929     C***                EVAL      InSepara = '+'
067700160929     C***                EXSR      SR_SPLIT
067800160929     C***                EVAL      savRMO = %trim(SkSplitCSV_5(3))
067900160929     C***                EVAL      savCMO = %trim(SkSplitCSV_5(7))
068000160929     C***                EVAL      savNMO = %trim(SkSplitCSV_5(8))
068100160929     C***                IF        savNMO = 'I'   OR
068200160929     C***                          savNMO = 'IT'  OR
068300160929     C***                          savNMO = 'ITA'
068400160929     C***                EVAL      savNMO = *blanks
068500160929     C***                ELSE
068600160929     C***                Z-ADD     1             jNAZ
068700160929     C***  savNMO        LOOKUP    skNAZISO(jNAZ)                         13
068800160929     C***                IF        %found
068900160929     C***                EVAL      savNMO = skNAZBAR(jNAZ)
069000160929     C***                ENDIF
069100160929     C***                ENDIF
069200160929     C***                ENDIF
069300150127     C*
069400150127     C***
069500150127     C* ...tipo record 'BGM+87' (info testata)
069600150127     C                   EVAL      wTag = 'BGM+87'
069700150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
069800150127     C                   MOVEL     *blanks       savNAS           15
069900150127     C                   EVAL      InStringa =
070000150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
070100150127     C                   EVAL      InSepara = '+'
070200150127     C                   EXSR      SR_SPLIT
070300150127     C                   EVAL      savNAS = %trim(SkSplitCSV_5(1))
070400150127     C                   ENDIF
070500141028     C*
070600141028     C***
070700150127     C* ...tipo record 'GID' (rottuta di codice spedizione)
070800150127     C                   EVAL      wTag = 'GID'
070900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
071000150127     C*
071100150127     C* Scarico testata in sospeso
071200150127     C                   IF        *in40
071300150127     C  N31              ADD       1             §CTROKVB
071400150127     C  N31              exsr      CHKWRI
071500160606     C  N31              exsr      WRIVATALL
071600150127     C  N31              WRITE     EDIVAB00
071700150127     C                   ELSE
071800150127     C                   SETON                                        40
071900150127     C                   ENDIF
072000150127     C*
072100150127     C* Inizio nuova bolla
072200141028     C                   SETOFF                                       31
072300141028     C                   eval      vinflg = '1'
072400141028     C* ......inizializzazioni iniziali e formati record file Bartolini
072500141028     C                   EXSR      INZVAR
072600141028     C* ......valorizzazione campi fissi
072700141028     C                   MOVEL     datcor        VABAAS
072800141028     C                   MOVE      datcor        VABMGS
072900141028     C                   MOVE(P)   vlrpoi        VABFGS
073000141028     C                   EVAL      VABCMR = savCMR
073100141028     C                   EVAL      VABDCM = datcor
073200141028     C                   EVAL      VABDTS = datcor
073300141028     C                   EVAL      VABCNT = 1
073400150127     C                   EVAL      VABRMO = savRMO
073500150127     C                   EVAL      VABCMO = savCMO
073600150127     C                   EVAL      VABNMO = savNMO
073700150127 xxx C                   EVAL      VABNAS = savNAS
073800141028     C* ......NSP => Stacco un numeratore da AZNUM
073900141028     C                   clear                   TRUL33DS
074000141028     C                   eval      I33OPE = *zeros
074100141028     C                   eval      I33CNU = 302
074200141028     C                   eval      I33NUM = 1
074300141028     C                   movel     TRUL33DS      KPJBU
074400141028     C                   call      'TRUL33R'
074500141028     C                   parm                    KPJBA
074600141028     C                   movel     KPJBU         TRUL33DS
074700141028     C                   if        O33ERR = *zeros
074800141028     C                   z-add     O33NRF        VABNSP
074900141028     C                   else
075000141028     C                   Z-ADD     1             wRecErr
075100141028     C                   SETON                                        31
075200141028     C                   EVAL      vinmsg = %trimr(vinmsg)
075300141028     C                             + ' ' + 'VABNSP VATNSP'
075400141028     C                   endif
075500141028     C                   ENDIF
075600150127     C*
075700150127     C***
075800150127     C* ...tipo record 'NAD+CN' (destinatario)
075900150127     C                   EVAL      wTag = 'NAD+CN'
076000150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
076100150127     C                   EVAL      InStringa =
076200150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
076300150127     C                   EVAL      InSepara = '+'
076400150127     C                   EXSR      SR_SPLIT
076500150921     C                   MOVEL     *blanks       wSgm2          1024
076600150921     C                   MOVEL     *blanks       wSgm4          1024
076700150921     C                   EVAL      wSgm2 = %trim(SkSplitCSV_5(2))
076800150921     C                   EVAL      wSgm4 = %trim(SkSplitCSV_5(4))
076900150127     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
077000150127     C***                EVAL      VABPRD = %trim(SkSplitCSV_5(6))
077100150127     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
077200150127     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
077300150127     C                   IF        VABNZD = 'I'   OR
077400150127     C                             VABNZD = 'IT'  OR
077500150127     C                             VABNZD = 'ITA'
077600150127     C                   EVAL      VABNZD = *blanks
077700150127     C                   ENDIF
077800150902     C*
077900150921     C                   MOVEL     *blanks       wRSD             70
078000150921     C                   EVAL      InStringa = wSgm2
078100150902     C                   EVAL      InSepara = ':'
078200150902     C                   EXSR      SR_SPLIT
078300150921     C
078400150921     C                   EVAL      wRSD = %trim(SkSplitCSV_5(1))
078500150921     C                   EVAL      VABRSD = %subst(wRSD:1:35)
078600150921     C                   EVAL      VABRD2 = %subst(wRSD:1+35:35)
078700150902     C*
078800150921     C                   EVAL      InStringa = wSgm4
078900150127     C                   EVAL      InSepara = ':'
079000150127     C                   EXSR      SR_SPLIT
079100160209     C                   IF        %trim(SkSplitCSV_5(2)) <> *blanks
079200160209     C                   EVAL      VABIND = %trim(SkSplitCSV_5(2))
079300160209     C                   EVAL      VABRD2 = %trim(%trim(VABRD2)+' '+
079400160209     C                                      %trim(SkSplitCSV_5(1)))
079500160209     C                   ELSE
079600150127     C                   EVAL      VABIND = %trim(SkSplitCSV_5(1))
079700160209     C                   ENDIF
079800150127     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
079900150127     C                                      %trim(SkSplitCSV_5(4))+
080000150127     C                                      %trim(SkSplitCSV_5(5))
080100150127     C                   ENDIF
080200141028     C*
080300141028     C***
080400150127     C* ...tipo record 'RFF+IV' (riferimento spedizione alfabetico)
080500150127     C                   EVAL      wTag = 'RFF+IV'
080600141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
080700141028     C                   EVAL      VABRMA =
080800141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
080900141028     C                   ENDIF
081000150127     C*
081100150127     C***
081200160613     C* ...tipo record 'RFF+CR' (riferimento spedizione numerico)
081300160606     C                   EVAL      wTag = 'RFF+CR'
081400150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081500150127     C                   EVAL      InStringa =
081600150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
081700150127     C                   EVAL      PiStr=InStringa
081800150127     C                   EXSR      CHKNUM
081900150127     C                   IF        PiNum=*on
082000150127     C                   Z-ADD     PiVal         VABRMN
082100150127     C                   ELSE
082200150127     C                   Z-ADD     1             wRecErr
082300150127     C                   ENDIF
082400150127     C                   ENDIF
082500150127     C*
082600150127     C***
082700160613     C* ...tipo record 'RFF+TB' ("chi sono" / Segnacollo Disk B)
082800160606     C                   EVAL      wTag = 'RFF+TB'
082900150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
083000150127     C                   EVAL      VATNOT =
083100160606     C                                %trim(%subst(wvindta:%len(%trim(wTag))+2))
083200160606     C*
083300160606     C* Verifico il tipo Disk corrrente
083400160606     C                   SELECT
083500160606     C*
083600160606     C                   WHEN      %len(%trim(VATNOT)) = 18                     * Disk B
083700160621     C*
083800160621     C* Verifica corretteza formale segnacollo BRT
083900160621     C                   CALL      'UBBRTSEGR'
084000160621     C                   PARM      VATNOT        pInBRT_SEGN
084100161018     C                   PARM      166           pInLNP01
084200161018     C                   PARM      *zeros        pInLNP02
084300161018     C                   PARM      *zeros        pInLNP03
084400161018     C                   PARM      *zeros        pInLNP04
084500161018     C                   PARM      *zeros        pInLNP05
084600161018     C                   PARM      03            pInNRS01
084700161018     C                   PARM      04            pInNRS02
084800161018     C                   PARM      29            pInNRS03
084900161018     C                   PARM      30            pInNRS04
085000161018     C                   PARM      34            pInNRS05
085100170605     C                   PARM      28            pInNRS06
085200170720     C                   PARM      94            pInNRS07
085300170720     C                   PARM      95            pInNRS08
085400170927     C                   PARM      96            pInNRS09
085500161018     C                   PARM      *zeros        pInNRS10
085600170928     C                   PARM      *zeros        pInNRS11
085700170928     C                   PARM      *zeros        pInNRS12
085800170928     C                   PARM      *zeros        pInNRS13
085900170928     C                   PARM      *zeros        pInNRS14
086000170928     C                   PARM      *zeros        pInNRS15
086100170928     C                   PARM      *zeros        pInNRS16
086200170928     C                   PARM      *zeros        pInNRS17
086300170928     C                   PARM      *zeros        pInNRS18
086400170928     C                   PARM      *zeros        pInNRS19
086500170928     C                   PARM      *zeros        pInNRS20
086600160621     C                   PARM      'S'           pInChkFil
086700160621     C                   PARM      'S'           pInChkDgt
086800160621     C                   PARM      *off          pOutEsitoOK
086900160621     C*
087000160621     C* Se esito controllo formale segnacollo BRT => OK
087100160623     C                   IF        pOutEsitoOK AND not *in55
087200160606     C                   EVAL      SEGNBRT = VATNOT
087300160620     C                   EVAL      VABLNP = SEGNBRT.LNP
087400160606     C                   EVAL      VABLNA = SEGNBRT.LNA
087500160606     C                   EVAL      VABNRS = SEGNBRT.NRS
087600160606     C                   EVAL      VABNCD = SEGNBRT.NCD
087700160606     C                   EVAL      VABNCA = SEGNBRT.NCD
087800160606     C                   EVAL      VABZNC = SEGNBRT.ZNC
087900160606     C                   SELECT
088000160606     C                   WHEN      SEGNBRT.LNA = 789                            * Disk B->C
088100160606     C                   EVAL      VABCTM = '2K'
088200160606     C                   OTHER
088300160606     C                   EVAL      VABCTM = '2'                                 * Disk B SEQ
088400160606     C                   ENDSL
088500160606     C                   CLEAR                   VATNOT
088600160621     C*
088700160621     C                   ELSE
088800160623     C  N55              Z-ADD     1             wRecErr
088900160623     C***                SETON                                        31
089000160623     C                   IF        %subst(VATNOT:1:3) <> '166'
089100160623     C                   EVAL      VATNOT = %editc(VABRMN:'X')
089200160623     C                   ENDIF
089300160623     C                   EVAL      VABCTM = '7Q'
089400160623     C                   EVAL      VATTRC = 'E'
089500160623     C                   EXSR      WRIVAT
089600160621     C                   ENDIF
089700160616     C*
089800160616     C                   WHEN      %len(%trim(VATNOT)) = *zeros                 * Attesa deploy DskB
089900160616     C                   EVAL      VATNOT = %editc(VABRMN:'X')
090000160616     C                   EVAL      VABCTM = '7Q'
090100160616     C                   EVAL      VATTRC = 'E'
090200160616     C                   EXSR      WRIVAT
090300160606     C*
090400160606     C                   OTHER                                                  * Disk C
090500160606     C                   EVAL      VABCTM = '7Q'
090600160606     C                   EVAL      VATTRC = 'E'
090700160606     C                   EXSR      WRIVAT
090800160606     C*
090900160606     C                   ENDSL
091000160606     C                   ADD       1             cntVABNCL
091100160606     C*
091200150127     C                   ENDIF
091300150127     C*
091400150127     C***
091500150127     C* ...tipo record 'RFF+TE' (telefono referente consegna + nome referente consegna)
091600150127     C                   EVAL      wTag = 'RFF+TE'
091700150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
091800160606     C                   EVAL      wVATNOT_B =
091900160606     C                                %subst(wvindta:%len(%trim(wTag))+2)
092000160606     C*
092100160606     C                   EVAL      wVATNOT_A = VABRSD
092200150127     C                   ENDIF
092300150127     C*
092400150127     C***
092500150127     C* ...tipo record 'TSR' (servizi speciali)
092600150127     C                   EVAL      wTag = 'TSR'
092700150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
092800150127     C                   EVAL      InStringa =
092900150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
093000150127     C                   EVAL      InSepara = '+'
093100150127     C                   EXSR      SR_SPLIT
093200150127     C                   SELECT
093300150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
093400150127     C                             %trim(SkSplitCSV_5(3)) = 'PRN'
093500150127     C                   EVAL      VABTSP = 'E'
093600150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
093700150127     C                             %trim(SkSplitCSV_5(3)) = '10H'
093800150127     C                   EVAL      VABTSP = 'H'
093900150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
094000150127     C                             %trim(SkSplitCSV_5(3)) = 'GWC'
094100150127     C                   EVAL      VABFFD = 'S'
094200150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
094300150127     C                             %trim(SkSplitCSV_5(3)) = 'BKI'
094400150127     C                   SELECT
094500150127     C                   WHEN      VABTC1 = *blanks
094600150127     C                   EVAL      VABTC1 = 'A'
094700150127     C                   WHEN      VABTC2 = *blanks
094800150127     C                   EVAL      VABTC2 = 'A'
094900150127     C                   ENDSL
095000150127     C                   ENDSL
095100150127     C                   ENDIF
095200150127     C*
095300150127     C***
095400150127     C* ...tipo record 'DTM+Z73' (servizi speciali)
095500150127     C                   EVAL      wTag = 'DTM+Z73'
095600150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
095700150127     C                   EVAL      InStringa =
095800150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
095900150127     C                   EVAL      InSepara = ':'
096000150127     C                   EXSR      SR_SPLIT
096100150127     C                   SELECT
096200150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '102'
096300150127     C                   EVAL      VABTCR = *blanks
096400150127     C                   EVAL      InStringa =
096500150127     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
096600150127     C                   EVAL      PiStr=InStringa
096700150127     C                   EXSR      CHKNUM
096800150127     C                   IF        PiNum=*on
096900150127     C                   Z-ADD     PiVal         VABDCR
097000150127     C                   ELSE
097100150127     C                   Z-ADD     1             wRecErr
097200150127     C                   ENDIF
097300150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '203'
097400150127     C                   EVAL      VABTCR = *blanks
097500150127     C                   EVAL      InStringa =
097600150127     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
097700150127     C                   EVAL      PiStr=InStringa
097800150127     C                   EXSR      CHKNUM
097900150127     C                   IF        PiNum=*on
098000150127     C                   Z-ADD     PiVal         VABDCR
098100150127     C                   ELSE
098200150127     C                   Z-ADD     1             wRecErr
098300150127     C                   ENDIF
098400150127     C                   EVAL      InStringa =
098500150127     C                                %subst(%trim(SkSplitCSV_5(1)):9:4)
098600150127     C                   EVAL      PiStr=InStringa
098700150127     C                   EXSR      CHKNUM
098800150127     C                   IF        PiNum=*on
098900150127     C                   Z-ADD     PiVal         VABHCR
099000150127     C                   ELSE
099100150127     C                   Z-ADD     1             wRecErr
099200150127     C                   ENDIF
099300150127     C                   ENDSL
099400150127     C                   ENDIF
099500141028     C*
099600141028     C***
099700141028     C* ...tipo record 'FTX+AAA' (natura merce)
099800150127     C***                EVAL      wTag = 'FTX+AAA'
099900150127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
100000150127     C***                EVAL      InStringa =
100100150127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
100200150127     C***                EVAL      InSepara = '+'
100300150127     C***                EXSR      SR_SPLIT
100400150127     C***                EVAL      VABNAS = %trim(SkSplitCSV_5(3))
100500150127     C***                ENDIF
100600141028     C*
100700141028     C***
100800150127     C* ...tipo record 'MEA+WX+B' (peso spedizione)
100900160126     C***                EVAL      wTag = 'MEA+WX+B'
101000160126     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
101100160126     C***                EVAL      InStringa =
101200160126     C***                             %subst(wvindta:%len(%trim(wTag))+2)
101300160126     C***                EVAL      InSepara = ':'
101400160126     C***                EXSR      SR_SPLIT
101500160126     C***                EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
101600160126     C***                EXSR      CHKNUM
101700160126     C***                IF        PiNum=*on
101800160126     C***                ADD       PiVal         VABPKB
101900160128     C***                IF        VABPKB = *zeros
102000160128     C***                EVAL      VABPKB = 0,1
102100160128     C***                ENDIF
102200160126     C***                ELSE
102300160126     C***                Z-ADD     1             wRecErr
102400160126     C***                ENDIF
102500160126     C***                ENDIF
102600160126     C*
102700160126     C***
102800160127     C* ...tipo record 'MEA+WT+G' (peso spedizione)
102900160127     C                   EVAL      wTag = 'MEA+WT+G'
103000160126     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
103100160126     C                   EVAL      InStringa =
103200160126     C                                %subst(wvindta:%len(%trim(wTag))+2)
103300160126     C                   EVAL      InSepara = ':'
103400160126     C                   EXSR      SR_SPLIT
103500160126     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
103600160126     C                   EXSR      CHKNUM
103700160126     C                   IF        PiNum=*on
103800160126     C                   ADD       PiVal         VABPKB
103900160128     C                   IF        VABPKB = *zeros
104000160128     C                   EVAL      VABPKB = 0,1
104100160128     C                   ENDIF
104200160126     C                   ELSE
104300160126     C                   Z-ADD     1             wRecErr
104400160126     C                   ENDIF
104500160126     C                   ENDIF
104600141028     C*
104700141028     C***
104800141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
104900150127     C***                EVAL      wTag = 'MEA+VOL'
105000150127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
105100150127     C***                EVAL      InStringa =
105200150127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
105300150127     C***                EVAL      InSepara = ':'
105400150127     C***                EXSR      SR_SPLIT
105500150127     C***                EVAL      PiStr=%trim(SkSplitCSV_5(2))
105600150127     C***                EXSR      CHKNUM
105700150127     C***                IF        PiNum=*on
105800150924     C***                ADD       PiVal         VABVLB
105900150127     C***                ELSE
106000150127     C***                Z-ADD     1             wRecErr
106100150127     C***                ENDIF
106200150127     C***                ENDIF
106300141028     C*
106400141028     C***
106500141028     C* ...tipo record '???' (PDF per packing-list)
106600141028     C                   EVAL      wTag = '???PDF'
106700141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
106800141028     C*
106900141028     C* ......VATNOT_P
107000141028     C                   if        *in53
107100141028     C*
107200141028     C* Solo se valorizzato RMN
107300141028     C                   if        VABRMA <> *blanks
107400141028     C                   eval      pIn_MaskPDF =
107500141028     C                                     %trim(%subst(wvindta:1292:100))
107600141028     C                   call      'TIS7P2SER'
107700141028     C                   parm      'W'           pIn_Opz           1
107800141028     C                   parm                    tivlrds
107900141028     C                   parm                    EDIVABDS
108000141028     C                   parm      '10'          pIn_CdIdAz        2
108100141028     C                   parm                    pIn_MaskPDF      50
108200141028     C                   parm      *blanks       pOut_Esito        1
108300141028     C                   endif
108400141028     C                   endif
108500141028     C*
108600141028     C                   ENDIF
108700141028     C*
108800141028     C***
108900150127     C* ...tipo record 'UNT' (fine bolla)
109000141028     C                   EVAL      wTag = 'UNT'
109100141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
109200141028     C  N31              ADD       1             §CTROKVB
109300141028     C  N31              exsr      CHKWRI
109400160606     C  N31              exsr      WRIVATALL
109500140117     C  N31              WRITE     EDIVAB00
109600100722     C                   endif
109700010202     C*
109800000801     C* Ebbene...
109900000801     C                   ADD       1             §CTRMO
110000141028     C                   IF        wRecErr <> *zeros
110100000801     C                   ADD       1             §CTRNO
110200000801     C                   EVAL      vinflg = '2'
110300000801     C                   ENDIF
110400000801     C*
110500000801     C                   ENDSR
110600010202     C*----------------------------------------------------*
110700140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
110800010202     C*----------------------------------------------------*
110900020305     C     PREVAT        BEGSR
111000010202     C*
111100140117     C* Compongo il nome del membro da dare al EDIVATWR
111200010202     C                   eval      parmbr = vlrhdl
111300010202     C                   movel     'M'           parmbr
111400060113     C                   eval      parccm = vlrksc
111500010202     C                   eval      paropz = '1'
111600010202     C* Effettuo la chiamata al CLLE preposto
111700140117     C                   call(e)   'TITVEVTC'
111800010202     C                   parm                    parccm
111900010202     C                   parm                    parmbr
112000010202     C                   parm                    paropz
112100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
112200010202     C                   if        %error
112300010202     C                   movel     '1'           chkcall
112400010202     C                   else
112500010202     C                   movel     '0'           chkcall
112600010202     C                   endif
112700010202     C*
112800010202     C                   ENDSR
112900141028     C*----------------------------------------------------*
113000141028     C*  CONTROLLO NUMERICITA' CAMPI
113100141028     C*----------------------------------------------------*
113200141028     C     CHKNUM        BEGSR
113300141028     C*
113400141028     C                   IF        PiDecChr = *blanks
113500141028     C                   EVAL      PiDecChr = CharNUM
113600141028     C                   ENDIF
113700141028     C*
113800141028     C                   callp     UBISNUM_Check(PiStr
113900141028     C                                          :PiDecChr
114000141028     C                                          :PiVal
114100141028     C                                          :PiNum
114200141028     C                                          :PiInt)
114300141028     C*
114400141028     C                   ENDSR
114500141028     C***
114600141028
114700141028
114800141028     C*----------------------------------------------------*
114900141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
115000141028     C*----------------------------------------------------*
115100141028     C     SR_SPLIT      BEGSR
115200141028     C*
115300141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
115400141028     C                   CALL      'XSPLIT2'
115500141028     C                   PARM                    InStringa
115600141028     C                   PARM                    InSepara
115700141028     C                   PARM      '5'           InTypeOut
115800141028     C                   PARM      ''            wSkParam
115900141028     C                   PARM                    OutErrore
116000141028     C                   PARM                    OutMsg
116100141028     C                   MOVEA     wSkParam      SkSplitCSV_5
116200141028     C*
116300141028     C                   ENDSR
116400141028
116500141028
116600141028     C*----------------------------------------------------*
116700141028     C*  SCRITTURA BUFFER EDIVAT
116800141028     C*----------------------------------------------------*
116900141028     C     WRIVAT        BEGSR
117000141028     C*
117100141028     C                   IF        VATNOT <> *blanks
117200141028     C                   EVAL      VATFGS = VABFGS
117300141028     C                   EVAL      VATCCM = VABCCM
117400141028     C                   EVAL      VATLNP = VABLNP
117500141028     C                   EVAL      VATAAS = VABAAS
117600141028     C                   EVAL      VATNRS = VABNRS
117700141028     C                   EVAL      VATNSP = VABNSP
117800141028     C                   EVAL      VATCMR = VABCMR
117900141028     C                   EVAL      VATCNT = VABCNT
118000141028     C                   ADD       1             §CTROKVT
118100141028     C                   WRITE     EDIVAT00
118200141028     C                   ENDIF
118300141028     C*
118400141028     C                   ENDSR
118500160606
118600160606
118700160606     C*----------------------------------------------------*
118800160606     C*  SCRITTURA BUFFER EDIVAT - TUTTI TIPI RECORD
118900160606     C*----------------------------------------------------*
119000160606     C     WRIVATALL     BEGSR
119100160606     C*
119200160606     C                   EVAL      VATFGS = VABFGS
119300160606     C                   EVAL      VATCCM = VABCCM
119400160606     C                   EVAL      VATLNP = VABLNP
119500160606     C                   EVAL      VATAAS = VABAAS
119600160606     C                   EVAL      VATNRS = VABNRS
119700160606     C                   EVAL      VATNSP = VABNSP
119800160606     C                   EVAL      VATCMR = VABCMR
119900160606     C                   EVAL      VATCNT = VABCNT
120000160606     C*
120100160606     C                   IF        wVATNOT_A <> *blanks
120200160606     C                   EVAL      VATNOT = %trim(wVATNOT_A)
120300160606     C                   EVAL      VATTRC = 'A'
120400160606     C                   WRITE     EDIVAT00
120500160606     C                   ADD       1             §CTROKVT
120600160606     C                   ENDIF
120700160606     C*
120800160606     C                   IF        wVATNOT_B <> *blanks
120900160606     C                   EVAL      VATNOT = %trim(wVATNOT_B)
121000160606     C                   EVAL      VATTRC = 'B'
121100160606     C                   WRITE     EDIVAT00
121200160606     C                   ADD       1             §CTROKVT
121300160606     C                   ENDIF
121400160606     C*
121500160606     C                   ENDSR
121600000801
121700011113
121800011113     C*----------------------------------------------------*
121900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
122000011113     C*----------------------------------------------------*
122100011113     C     CHKIMPDIV     BEGSR
122200011113     C*
122300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
122400011113     C                   Z-ADD     *zeros        wrkDec            9 9
122500011113     C*
122600011113     C* Come prima cosa effettuo considerazioni sulla divisa
122700011113     C                   IF        vabIAS > *zeros
122800011113     C                   IF        vabVAS <> 'EUR'
122900011113     C                   EVAL      vabVAS =  'ITL'
123000011113     C                   ENDIF
123100011113     C                   ENDIF
123200011113     C*
123300011113     C                   IF        vabCAS > *zeros
123400011113     C                   IF        vabVCA <> 'EUR'
123500011113     C                   EVAL      vabVCA =  'ITL'
123600011113     C                   ENDIF
123700011113     C                   ENDIF
123800011113     C*
123900011113     C                   IF        vabVMD > *zeros
124000020305     C                   IF        vabVAD <> 'EUR'
124100011113     C                   EVAL      vabVAD =  'ITL'
124200011113     C                   ENDIF
124300011113     C                   ENDIF
124400011113     C*
124500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
124600011113     C                   Z-ADD     vabIAS        wrkDec
124700011113     C                   IF        wrkDec > *zeros
124800011113     C                   IF        vabVAS = 'ITL'
124900011113     C                   EVAL      vabIAS = *zeros
125000011113     C                   ENDIF
125100011113     C                   ENDIF
125200011113     C*
125300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
125400011113     C                   Z-ADD     vabCAS        wrkDec
125500011113     C                   IF        wrkDec > *zeros
125600011113     C                   IF        vabVCA = 'ITL'
125700011113     C                   EVAL      vabCAS = *zeros
125800011113     C                   ENDIF
125900011113     C                   ENDIF
126000011113     C*
126100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
126200011113     C                   Z-ADD     vabVMD        wrkDec
126300011113     C                   IF        wrkDec > *zeros
126400011113     C                   IF        vabVAD = 'ITL'
126500011113     C                   EVAL      vabVMD = *zeros
126600011113     C                   ENDIF
126700011113     C                   ENDIF
126800011113     C*
126900011113     C                   ENDSR
127000011113     C***
127100011113
127200011113
127300000801
127400000801
127500990920      /TITLE Invio dei dati al punto operativo.
127600010202     C     invio         BEGSR
127700990920     C*
127800140117     C* 1° invio EDIVAT
127900010201     C                   reset                   dscmz
128000010201     C                   move      vlrpoi        cmzdst
128100140117     C                   eval      cmzfld = 'EDIVATWR'
128200010201     C                   eval      cmzmbd = vlrhdl
128300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
128400021009     C***                if        prmfir = *blanks
128500140117     C                   eval      cmzfla = 'EDIVAT0F'
128600140117     C                   eval      cmzmba = 'EDIVAT0F'
128700021009     C***                else
128800021009     C***                eval      cmzfla = prmfir
128900021009     C***                eval      cmzmba = prmfir
129000021009     C***                endif
129100010201     C                   eval      cmznrr = *zeros
129200020305     C                   move      §ctrokvt      cmznrr
129300021018     C                   eval      cmzlba = vlrfl1
129400010201     C                   call(e)   'TIS711C'
129500010201     C                   parm                    dscmz
129600010201     C                   parm      *blanks       esito
129700010205     C                   if        %error
129800010205     C                             or cmzerr = '1'
129900010205     C                             or esito  = '1'
130000010205     C                   eval      wrkesito = '3'
130100010205     C                   else
130200010201     C*
130300140117     C* 2° invio EDIVAB
130400010201     C                   reset                   dscmz
130500010201     C                   move      vlrpoi        cmzdst
130600010201     C                   eval      cmzfld = vlrfou
130700010201     C                   eval      cmzmbd = vlrhdl
130800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
130900021009     C***                if        prmfir = *blanks
131000140117     C                   eval      cmzfla = 'EDIVAB0F'
131100140117     C                   eval      cmzmba = 'EDIVAB0F'
131200021009     C***                else
131300021009     C***                eval      cmzfla = prmfir
131400021009     C***                eval      cmzmba = prmfir
131500021009     C***                endif
131600010201     C                   eval      cmznrr = *zeros
131700010201     C                   move      §ctrokvb      cmznrr
131800021018     C                   eval      cmzlba = vlrfl1
131900010201     C                   call(e)   'TIS711C'
132000010201     C                   parm                    dscmz
132100010201     C                   parm      *blanks       esito
132200010201     C                   if        %error
132300010201     C                             or cmzerr = '1'
132400010201     C                             or esito  = '1'
132500010201     C                   eval      wrkesito = '3'
132600010201     C                   endif
132700010205     C                   endif
132800990920     C*
132900000613     C                   ENDSR
133000000613     C***
133100070411
133200141028
133300070411     C     *pssr         BEGSR
133400070411     C*
133500070411     C                   if        %open(tivin00r)
133600070411     C                   close     tivin00r
133700070411     C                   endif
133800140117     C                   if        %open(edivabwr)
133900140117     C                   close     edivabwr
134000070411     C                   endif
134100140117     C                   if        %open(edivatwr)
134200140117     C                   close     edivatwr
134300070411     C                   endif
134400070411     C*
134500070411     C* Effettuo la chiamata al CLLE preposto
134600140117     C                   call(e)   'TITVEVTC'
134700070411     C                   parm                    parccm
134800070411     C                   parm                    parmbr
134900070411     C                   parm      '2'           paropz
135000070411     C*
135100070411     C                   eval      wrkesito = '2'
135200070411     C*
135300070411     C                   seton                                        LR
135400070411     C*
135500070411     C                   ENDSR     '*CANCL'
135600070411     C***
135700160801
135800160801
135900160801     C*--------------------------------------------------------
136000160801     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
136100160801     C*--------------------------------------------------------
136200160801     C     CARTAB        BEGSR
136300160801     C*
136400160801     C* TABELLA '15' - NAZIONI
136500160801     C                   clear                   skNAZISO
136600160801     C                   clear                   skNAZBAR
136700160801     C                   eval      tblKUT = 1
136800160801     C                   eval      tblCOD = '15'
136900160801     C     KEYtabP       setll     tabel00f
137000160801     C     KEYtabP       reade     tabel00f
137100160801     C                   dow       not %eof(tabel00f)
137200160801     C                   if        tblFLG = *blanks
137300160801     C                   movel(p)  tblUNI        ds15
137400160801     C                   add       1             jNAZ
137500160801     C                   eval      skNAZBAR(jNAZ) = tblKEY
137600160801     C                   eval      skNAZISO(jNAZ) = §15COD
137700160801     C                   endif
137800160801     C     KEYtabP       reade     tabel00f
137900160801     C                   enddo
138000160801     C*
138100160801     C                   ENDSR
138200160801     C***
138300070411
138400990910
138500000613     C     *inzsr        BEGSR
138600990910     C*
138700990910     C     *entry        plist
138800990920     C                   parm                    tivlrds
138900990921     C                   parm      wrkesito      esito
139000000724     C                   parm                    prmlit
139100000710     C                   parm                    prmfir
139200000613     C*
139300000830     C* CALCOLA LA DATA CORRENTE
139400140117     C                   time                    wn14             14 0
139500140117     C                   movel     wn14          oracor            6 0          *ORA
139600100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
139700100722     C                   eval      datcor = %dec(%date() : *ISO)
139800160801     C*
139900160801     C* Chiave su TABEL00F - parziale
140000160801     C     KEYtabP       KLIST
140100160801     C                   KFLD                    tblKUT
140200160801     C                   KFLD                    tblCOD
140300000830     C*
140400000613     C                   ENDSR
140500000613     C***
