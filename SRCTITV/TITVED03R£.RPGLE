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
003800141028
003900160801     D*------------
004000160801     D jNAZ            s              5  0 INZ(*zeros)
004100160801     D skNAZISO        S              3    DIM(1000)
004200160801     D skNAZBAR        S              3    DIM(1000)
004300160601
004400160601     D*------------------
004500160601     D* DS RIDEFINIZIONE SEGNACOLLO BRT
004600160601     D*------------------
004700160601     D SEGNBRT         ds                  inz qualified
004800160620     D  LNP                           3  0
004900160601     D  LNA                           3  0
005000160601     D  NRS                           2  0
005100160601     D  NCD                           7  0
005200160601     D  ZNC                           2  0
005300160601     D  CHKDGT                        1
005400160601
005500160601
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
066900160809     C***                EVAL      wTag = 'NAD+SF'
067000160809     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
067100160809     C                   MOVEL     *blanks       savRMO           25
067200160809     C                   MOVEL     *blanks       savCMO            9
067300160809     C                   MOVEL     *blanks       savNMO            3
067400160809     C***                EVAL      InStringa =
067500160809     C***                             %subst(wvindta:%len(%trim(wTag))+2)
067600160809     C***                EVAL      InSepara = '+'
067700160809     C***                EXSR      SR_SPLIT
067800160809     C***                EVAL      savRMO = %trim(SkSplitCSV_5(3))
067900160809     C***                EVAL      savCMO = %trim(SkSplitCSV_5(7))
068000160809     C***                EVAL      savNMO = %trim(SkSplitCSV_5(8))
068100160809     C***                IF        savNMO = 'I'   OR
068200160809     C***                          savNMO = 'IT'  OR
068300160809     C***                          savNMO = 'ITA'
068400160809     C***                EVAL      savNMO = *blanks
068500160809     C***                ELSE
068600160809     C***                Z-ADD     1             jNAZ
068700160809     C***  savNMO        LOOKUP    skNAZISO(jNAZ)                         13
068800160809     C***                IF        %found
068900160809     C***                EVAL      savNMO = skNAZBAR(jNAZ)
069000160809     C***                ENDIF
069100160809     C***                ENDIF
069200160809     C***                ENDIF
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
073700160809     C                   EVAL      VABNAS = savNAS
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
076500150127     C                   EVAL      VABRSD = %trim(SkSplitCSV_5(2))
076600150127     C                   EVAL      VABLOD = %trim(SkSplitCSV_5(5))
076700150127     C***                EVAL      VABPRD = %trim(SkSplitCSV_5(6))
076800150127     C                   EVAL      VABCAD = %trim(SkSplitCSV_5(7))
076900150127     C                   EVAL      VABNZD = %trim(SkSplitCSV_5(8))
077000150127     C                   IF        VABNZD = 'I'   OR
077100150127     C                             VABNZD = 'IT'  OR
077200150127     C                             VABNZD = 'ITA'
077300150127     C                   EVAL      VABNZD = *blanks
077400150127     C                   ENDIF
077500150127     C                   EVAL      InStringa = %trim(SkSplitCSV_5(4))
077600150127     C                   EVAL      InSepara = ':'
077700150127     C                   EXSR      SR_SPLIT
077800150127     C                   EVAL      VABIND = %trim(SkSplitCSV_5(1))
077900150127     C                   EVAL      VABRD2 = %trim(SkSplitCSV_5(2))
078000150127     C                   EVAL      VABNOT = %trim(SkSplitCSV_5(3))+
078100150127     C                                      %trim(SkSplitCSV_5(4))+
078200150127     C                                      %trim(SkSplitCSV_5(5))
078300150127     C                   ENDIF
078400141028     C*
078500141028     C***
078600150127     C* ...tipo record 'RFF+IV' (riferimento spedizione alfabetico)
078700150127     C                   EVAL      wTag = 'RFF+IV'
078800141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
078900141028     C                   EVAL      VABRMA =
079000141028     C                                %subst(wvindta:%len(%trim(wTag))+2)
079100141028     C                   ENDIF
079200150127     C*
079300150127     C***
079400160613     C* ...tipo record 'RFF+CR' (riferimento spedizione numerico)
079500160601     C                   EVAL      wTag = 'RFF+CR'
079600150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
079700150127     C                   EVAL      InStringa =
079800150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
079900150127     C                   EVAL      PiStr=InStringa
080000150127     C                   EXSR      CHKNUM
080100150127     C                   IF        PiNum=*on
080200150127     C                   Z-ADD     PiVal         VABRMN
080300150127     C                   ELSE
080400150127     C                   Z-ADD     1             wRecErr
080500150127     C                   ENDIF
080600150127     C                   ENDIF
080700150127     C*
080800150127     C***
080900160613     C* ...tipo record 'RFF+TB' ("chi sono" / Segnacollo Disk B)
081000160601     C                   EVAL      wTag = 'RFF+TB'
081100150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
081200150127     C                   EVAL      VATNOT =
081300160601     C                                %trim(%subst(wvindta:%len(%trim(wTag))+2))
081400160601     C*
081500160601     C* Verifico il tipo Disk corrrente
081600160601     C                   SELECT
081700160601     C*
081800160601     C                   WHEN      %len(%trim(VATNOT)) = 18                     * Disk B
081900160621     C*
082000160621     C* Verifica corretteza formale segnacollo BRT
082100160621     C                   CALL      'UBBRTSEGR'
082200160621     C                   PARM      VATNOT        pInBRT_SEGN
082300161018     C                   PARM      166           pInLNP01
082400161018     C                   PARM      *zeros        pInLNP02
082500161018     C                   PARM      *zeros        pInLNP03
082600161018     C                   PARM      *zeros        pInLNP04
082700161018     C                   PARM      *zeros        pInLNP05
082800161018     C                   PARM      03            pInNRS01
082900161018     C                   PARM      04            pInNRS02
083000161018     C                   PARM      29            pInNRS03
083100161018     C                   PARM      30            pInNRS04
083200161018     C                   PARM      34            pInNRS05
083300170427     C                   PARM      28            pInNRS06
083400170720     C                   PARM      94            pInNRS07
083500170720     C                   PARM      95            pInNRS08
083600170927     C                   PARM      96            pInNRS09
083700161018     C                   PARM      *zeros        pInNRS10
083800170928     C                   PARM      *zeros        pInNRS11
083900170928     C                   PARM      *zeros        pInNRS12
084000170928     C                   PARM      *zeros        pInNRS13
084100170928     C                   PARM      *zeros        pInNRS14
084200170928     C                   PARM      *zeros        pInNRS15
084300170928     C                   PARM      *zeros        pInNRS16
084400170928     C                   PARM      *zeros        pInNRS17
084500170928     C                   PARM      *zeros        pInNRS18
084600170928     C                   PARM      *zeros        pInNRS19
084700170928     C                   PARM      *zeros        pInNRS20
084800160621     C                   PARM      'S'           pInChkFil
084900160621     C                   PARM      'S'           pInChkDgt
085000160621     C                   PARM      *off          pOutEsitoOK
085100160621     C*
085200160621     C* Se esito controllo formale segnacollo BRT => OK
085300160623     C                   IF        pOutEsitoOK AND not *in55
085400160601     C                   EVAL      SEGNBRT = VATNOT
085500160620     C                   EVAL      VABLNP = SEGNBRT.LNP
085600160601     C                   EVAL      VABLNA = SEGNBRT.LNA
085700160601     C                   EVAL      VABNRS = SEGNBRT.NRS
085800160601     C                   EVAL      VABNCD = SEGNBRT.NCD
085900160606     C                   EVAL      VABNCA = SEGNBRT.NCD
086000160601     C                   EVAL      VABZNC = SEGNBRT.ZNC
086100160601     C                   SELECT
086200160601     C                   WHEN      SEGNBRT.LNA = 789                            * Disk B->C
086300160601     C                   EVAL      VABCTM = '2K'
086400160601     C                   OTHER
086500160601     C                   EVAL      VABCTM = '2'                                 * Disk B SEQ
086600160601     C                   ENDSL
086700160601     C                   CLEAR                   VATNOT
086800160621     C*
086900160621     C                   ELSE
087000160623     C  N55              Z-ADD     1             wRecErr
087100160623     C***                SETON                                        31
087200160623     C                   IF        %subst(VATNOT:1:3) <> '166'
087300160623     C                   EVAL      VATNOT = %editc(VABRMN:'X')
087400160623     C                   ENDIF
087500160623     C                   EVAL      VABCTM = '7Q'
087600160623     C                   EVAL      VATTRC = 'E'
087700160623     C                   EXSR      WRIVAT
087800160621     C                   ENDIF
087900160601     C*
088000160616     C                   WHEN      %len(%trim(VATNOT)) = *zeros                 * Attesa deploy DskB
088100160616     C                   EVAL      VATNOT = %editc(VABRMN:'X')
088200160616     C                   EVAL      VABCTM = '7Q'
088300160616     C                   EVAL      VATTRC = 'E'
088400160616     C                   EXSR      WRIVAT
088500160616     C*
088600160601     C                   OTHER                                                  * Disk C
088700160601     C                   EVAL      VABCTM = '7Q'
088800160601     C                   EVAL      VATTRC = 'E'
088900160601     C                   EXSR      WRIVAT
089000160601     C*
089100160601     C                   ENDSL
089200160606     C                   ADD       1             cntVABNCL
089300160601     C*
089400150127     C                   ENDIF
089500150127     C*
089600150127     C***
089700150127     C* ...tipo record 'RFF+TE' (telefono referente consegna + nome referente consegna)
089800150127     C                   EVAL      wTag = 'RFF+TE'
089900150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
090000160606     C                   EVAL      wVATNOT_B =
090100150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
090200150127     C*
090300160606     C                   EVAL      wVATNOT_A = VABRSD
090400150127     C                   ENDIF
090500150127     C*
090600150127     C***
090700150127     C* ...tipo record 'TSR' (servizi speciali)
090800150127     C                   EVAL      wTag = 'TSR'
090900150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
091000150127     C                   EVAL      InStringa =
091100150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
091200150127     C                   EVAL      InSepara = '+'
091300150127     C                   EXSR      SR_SPLIT
091400150127     C                   SELECT
091500150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
091600150127     C                             %trim(SkSplitCSV_5(3)) = 'PRN'
091700150127     C                   EVAL      VABTSP = 'E'
091800150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
091900150127     C                             %trim(SkSplitCSV_5(3)) = '10H'
092000150127     C                   EVAL      VABTSP = 'H'
092100150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
092200150127     C                             %trim(SkSplitCSV_5(3)) = 'GWC'
092300150127     C                   EVAL      VABFFD = 'S'
092400150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '3' and
092500150127     C                             %trim(SkSplitCSV_5(3)) = 'BKI'
092600150127     C                   SELECT
092700150127     C                   WHEN      VABTC1 = *blanks
092800150127     C                   EVAL      VABTC1 = 'A'
092900150127     C                   WHEN      VABTC2 = *blanks
093000150127     C                   EVAL      VABTC2 = 'A'
093100150127     C                   ENDSL
093200150127     C                   ENDSL
093300150127     C                   ENDIF
093400150127     C*
093500150127     C***
093600150127     C* ...tipo record 'DTM+Z73' (servizi speciali)
093700150127     C                   EVAL      wTag = 'DTM+Z73'
093800150127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
093900150127     C                   EVAL      InStringa =
094000150127     C                                %subst(wvindta:%len(%trim(wTag))+2)
094100150127     C                   EVAL      InSepara = ':'
094200150127     C                   EXSR      SR_SPLIT
094300150127     C                   SELECT
094400150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '102'
094500150127     C                   EVAL      VABTCR = *blanks
094600150127     C                   EVAL      InStringa =
094700150127     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
094800150127     C                   EVAL      PiStr=InStringa
094900150127     C                   EXSR      CHKNUM
095000150127     C                   IF        PiNum=*on
095100150127     C                   Z-ADD     PiVal         VABDCR
095200150127     C                   ELSE
095300150127     C                   Z-ADD     1             wRecErr
095400150127     C                   ENDIF
095500150127     C                   WHEN      %trim(SkSplitCSV_5(2)) = '203'
095600150127     C                   EVAL      VABTCR = *blanks
095700150127     C                   EVAL      InStringa =
095800150127     C                                %subst(%trim(SkSplitCSV_5(1)):1:8)
095900150127     C                   EVAL      PiStr=InStringa
096000150127     C                   EXSR      CHKNUM
096100150127     C                   IF        PiNum=*on
096200150127     C                   Z-ADD     PiVal         VABDCR
096300150127     C                   ELSE
096400150127     C                   Z-ADD     1             wRecErr
096500150127     C                   ENDIF
096600150127     C                   EVAL      InStringa =
096700150127     C                                %subst(%trim(SkSplitCSV_5(1)):9:4)
096800150127     C                   EVAL      PiStr=InStringa
096900150127     C                   EXSR      CHKNUM
097000150127     C                   IF        PiNum=*on
097100150127     C                   Z-ADD     PiVal         VABHCR
097200150127     C                   ELSE
097300150127     C                   Z-ADD     1             wRecErr
097400150127     C                   ENDIF
097500150127     C                   ENDSL
097600150127     C                   ENDIF
097700141028     C*
097800141028     C***
097900141028     C* ...tipo record 'FTX+AAA' (natura merce)
098000150127     C***                EVAL      wTag = 'FTX+AAA'
098100150127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
098200150127     C***                EVAL      InStringa =
098300150127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
098400150127     C***                EVAL      InSepara = '+'
098500150127     C***                EXSR      SR_SPLIT
098600150127     C***                EVAL      VABNAS = %trim(SkSplitCSV_5(3))
098700150127     C***                ENDIF
098800141028     C*
098900141028     C***
099000150127     C* ...tipo record 'MEA+WX+B' (peso spedizione)
099100160127     C***                EVAL      wTag = 'MEA+WX+B'
099200160127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
099300160127     C***                EVAL      InStringa =
099400160127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
099500160127     C***                EVAL      InSepara = ':'
099600160127     C***                EXSR      SR_SPLIT
099700160127     C***                EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
099800160127     C***                EXSR      CHKNUM
099900160127     C***                IF        PiNum=*on
100000160127     C***                ADD       PiVal         VABPKB
100100160128     C***                IF        VABPKB = *zeros
100200160128     C***                EVAL      VABPKB = 0,1
100300160128     C***                ENDIF
100400160127     C***                ELSE
100500160127     C***                Z-ADD     1             wRecErr
100600160127     C***                ENDIF
100700160127     C***                ENDIF
100800160127     C*
100900160127     C***
101000160127     C* ...tipo record 'MEA+WT+G' (peso spedizione)
101100160127     C                   EVAL      wTag = 'MEA+WT+G'
101200160127     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
101300160127     C                   EVAL      InStringa =
101400160127     C                                %subst(wvindta:%len(%trim(wTag))+2)
101500160127     C                   EVAL      InSepara = ':'
101600160127     C                   EXSR      SR_SPLIT
101700160127     C                   EVAL      PiStr='0'+%trim(SkSplitCSV_5(2))
101800160127     C                   EXSR      CHKNUM
101900160127     C                   IF        PiNum=*on
102000160127     C                   ADD       PiVal         VABPKB
102100160128     C                   IF        VABPKB = *zeros
102200160128     C                   EVAL      VABPKB = 0,1
102300160128     C                   ENDIF
102400160127     C                   ELSE
102500160127     C                   Z-ADD     1             wRecErr
102600160127     C                   ENDIF
102700160127     C                   ENDIF
102800141028     C*
102900141028     C***
103000141028     C* ...tipo record 'MEA+VOL' (volume spedizione)
103100150127     C***                EVAL      wTag = 'MEA+VOL'
103200150127     C***                IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
103300150127     C***                EVAL      InStringa =
103400150127     C***                             %subst(wvindta:%len(%trim(wTag))+2)
103500150127     C***                EVAL      InSepara = ':'
103600150127     C***                EXSR      SR_SPLIT
103700150127     C***                EVAL      PiStr=%trim(SkSplitCSV_5(2))
103800150127     C***                EXSR      CHKNUM
103900150127     C***                IF        PiNum=*on
104000150924     C***                ADD       PiVal         VABVLB
104100150127     C***                ELSE
104200150127     C***                Z-ADD     1             wRecErr
104300150127     C***                ENDIF
104400150127     C***                ENDIF
104500141028     C*
104600141028     C***
104700141028     C* ...tipo record '???' (PDF per packing-list)
104800141028     C                   EVAL      wTag = '???PDF'
104900141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
105000141028     C*
105100141028     C* ......VATNOT_P
105200141028     C                   if        *in53
105300141028     C*
105400141028     C* Solo se valorizzato RMN
105500141028     C                   if        VABRMA <> *blanks
105600141028     C                   eval      pIn_MaskPDF =
105700141028     C                                     %trim(%subst(wvindta:1292:100))
105800141028     C                   call      'TIS7P2SER'
105900141028     C                   parm      'W'           pIn_Opz           1
106000141028     C                   parm                    tivlrds
106100141028     C                   parm                    EDIVABDS
106200141028     C                   parm      '10'          pIn_CdIdAz        2
106300141028     C                   parm                    pIn_MaskPDF      50
106400141028     C                   parm      *blanks       pOut_Esito        1
106500141028     C                   endif
106600141028     C                   endif
106700141028     C*
106800141028     C                   ENDIF
106900141028     C*
107000141028     C***
107100150127     C* ...tipo record 'UNT' (fine bolla)
107200141028     C                   EVAL      wTag = 'UNT'
107300141028     C                   IF        %subst(wvindta:1:%len(%trim(wTag))) = wTag
107400141028     C  N31              ADD       1             §CTROKVB
107500141028     C  N31              exsr      CHKWRI
107600160606     C  N31              exsr      WRIVATALL
107700140117     C  N31              WRITE     EDIVAB00
107800100722     C                   endif
107900010202     C*
108000000801     C* Ebbene...
108100000801     C                   ADD       1             §CTRMO
108200141028     C                   IF        wRecErr <> *zeros
108300000801     C                   ADD       1             §CTRNO
108400000801     C                   EVAL      vinflg = '2'
108500000801     C                   ENDIF
108600000801     C*
108700000801     C                   ENDSR
108800010202     C*----------------------------------------------------*
108900140117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
109000010202     C*----------------------------------------------------*
109100020305     C     PREVAT        BEGSR
109200010202     C*
109300140117     C* Compongo il nome del membro da dare al EDIVATWR
109400010202     C                   eval      parmbr = vlrhdl
109500010202     C                   movel     'M'           parmbr
109600060113     C                   eval      parccm = vlrksc
109700010202     C                   eval      paropz = '1'
109800010202     C* Effettuo la chiamata al CLLE preposto
109900140117     C                   call(e)   'TITVEVTC'
110000010202     C                   parm                    parccm
110100010202     C                   parm                    parmbr
110200010202     C                   parm                    paropz
110300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
110400010202     C                   if        %error
110500010202     C                   movel     '1'           chkcall
110600010202     C                   else
110700010202     C                   movel     '0'           chkcall
110800010202     C                   endif
110900010202     C*
111000010202     C                   ENDSR
111100141028     C*----------------------------------------------------*
111200141028     C*  CONTROLLO NUMERICITA' CAMPI
111300141028     C*----------------------------------------------------*
111400141028     C     CHKNUM        BEGSR
111500141028     C*
111600141028     C                   IF        PiDecChr = *blanks
111700141028     C                   EVAL      PiDecChr = CharNUM
111800141028     C                   ENDIF
111900141028     C*
112000141028     C                   callp     UBISNUM_Check(PiStr
112100141028     C                                          :PiDecChr
112200141028     C                                          :PiVal
112300141028     C                                          :PiNum
112400141028     C                                          :PiInt)
112500141028     C*
112600141028     C                   ENDSR
112700141028     C***
112800141028
112900141028
113000141028     C*----------------------------------------------------*
113100141028     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
113200141028     C*----------------------------------------------------*
113300141028     C     SR_SPLIT      BEGSR
113400141028     C*
113500141028     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
113600141028     C                   CALL      'XSPLIT2'
113700141028     C                   PARM                    InStringa
113800141028     C                   PARM                    InSepara
113900141028     C                   PARM      '5'           InTypeOut
114000141028     C                   PARM      ''            wSkParam
114100141028     C                   PARM                    OutErrore
114200141028     C                   PARM                    OutMsg
114300141028     C                   MOVEA     wSkParam      SkSplitCSV_5
114400141028     C*
114500141028     C                   ENDSR
114600141028
114700141028
114800141028     C*----------------------------------------------------*
114900141028     C*  SCRITTURA BUFFER EDIVAT
115000141028     C*----------------------------------------------------*
115100141028     C     WRIVAT        BEGSR
115200141028     C*
115300141028     C                   IF        VATNOT <> *blanks
115400141028     C                   EVAL      VATFGS = VABFGS
115500141028     C                   EVAL      VATCCM = VABCCM
115600141028     C                   EVAL      VATLNP = VABLNP
115700141028     C                   EVAL      VATAAS = VABAAS
115800141028     C                   EVAL      VATNRS = VABNRS
115900141028     C                   EVAL      VATNSP = VABNSP
116000141028     C                   EVAL      VATCMR = VABCMR
116100141028     C                   EVAL      VATCNT = VABCNT
116200141028     C                   ADD       1             §CTROKVT
116300141028     C                   WRITE     EDIVAT00
116400141028     C                   ENDIF
116500141028     C*
116600141028     C                   ENDSR
116700160606
116800160606
116900160606     C*----------------------------------------------------*
117000160606     C*  SCRITTURA BUFFER EDIVAT - TUTTI TIPI RECORD
117100160606     C*----------------------------------------------------*
117200160606     C     WRIVATALL     BEGSR
117300160606     C*
117400160606     C                   EVAL      VATFGS = VABFGS
117500160606     C                   EVAL      VATCCM = VABCCM
117600160606     C                   EVAL      VATLNP = VABLNP
117700160606     C                   EVAL      VATAAS = VABAAS
117800160606     C                   EVAL      VATNRS = VABNRS
117900160606     C                   EVAL      VATNSP = VABNSP
118000160606     C                   EVAL      VATCMR = VABCMR
118100160606     C                   EVAL      VATCNT = VABCNT
118200160606     C*
118300160606     C                   IF        wVATNOT_A <> *blanks
118400160606     C                   EVAL      VATNOT = %trim(wVATNOT_A)
118500160606     C                   EVAL      VATTRC = 'A'
118600160606     C                   WRITE     EDIVAT00
118700160606     C                   ADD       1             §CTROKVT
118800160606     C                   ENDIF
118900160606     C*
119000160606     C                   IF        wVATNOT_B <> *blanks
119100160606     C                   EVAL      VATNOT = %trim(wVATNOT_B)
119200160606     C                   EVAL      VATTRC = 'B'
119300160606     C                   WRITE     EDIVAT00
119400160606     C                   ADD       1             §CTROKVT
119500160606     C                   ENDIF
119600160606     C*
119700160606     C                   ENDSR
119800000801
119900011113
120000011113     C*----------------------------------------------------*
120100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
120200011113     C*----------------------------------------------------*
120300011113     C     CHKIMPDIV     BEGSR
120400011113     C*
120500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
120600011113     C                   Z-ADD     *zeros        wrkDec            9 9
120700011113     C*
120800011113     C* Come prima cosa effettuo considerazioni sulla divisa
120900011113     C                   IF        vabIAS > *zeros
121000011113     C                   IF        vabVAS <> 'EUR'
121100011113     C                   EVAL      vabVAS =  'ITL'
121200011113     C                   ENDIF
121300011113     C                   ENDIF
121400011113     C*
121500011113     C                   IF        vabCAS > *zeros
121600011113     C                   IF        vabVCA <> 'EUR'
121700011113     C                   EVAL      vabVCA =  'ITL'
121800011113     C                   ENDIF
121900011113     C                   ENDIF
122000011113     C*
122100011113     C                   IF        vabVMD > *zeros
122200020305     C                   IF        vabVAD <> 'EUR'
122300011113     C                   EVAL      vabVAD =  'ITL'
122400011113     C                   ENDIF
122500011113     C                   ENDIF
122600011113     C*
122700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
122800011113     C                   Z-ADD     vabIAS        wrkDec
122900011113     C                   IF        wrkDec > *zeros
123000011113     C                   IF        vabVAS = 'ITL'
123100011113     C                   EVAL      vabIAS = *zeros
123200011113     C                   ENDIF
123300011113     C                   ENDIF
123400011113     C*
123500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
123600011113     C                   Z-ADD     vabCAS        wrkDec
123700011113     C                   IF        wrkDec > *zeros
123800011113     C                   IF        vabVCA = 'ITL'
123900011113     C                   EVAL      vabCAS = *zeros
124000011113     C                   ENDIF
124100011113     C                   ENDIF
124200011113     C*
124300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
124400011113     C                   Z-ADD     vabVMD        wrkDec
124500011113     C                   IF        wrkDec > *zeros
124600011113     C                   IF        vabVAD = 'ITL'
124700011113     C                   EVAL      vabVMD = *zeros
124800011113     C                   ENDIF
124900011113     C                   ENDIF
125000011113     C*
125100011113     C                   ENDSR
125200011113     C***
125300011113
125400011113
125500000801
125600000801
125700990920      /TITLE Invio dei dati al punto operativo.
125800010202     C     invio         BEGSR
125900990920     C*
126000140117     C* 1° invio EDIVAT
126100010201     C                   reset                   dscmz
126200010201     C                   move      vlrpoi        cmzdst
126300140117     C                   eval      cmzfld = 'EDIVATWR'
126400010201     C                   eval      cmzmbd = vlrhdl
126500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
126600021009     C***                if        prmfir = *blanks
126700140117     C                   eval      cmzfla = 'EDIVAT0F'
126800140117     C                   eval      cmzmba = 'EDIVAT0F'
126900021009     C***                else
127000021009     C***                eval      cmzfla = prmfir
127100021009     C***                eval      cmzmba = prmfir
127200021009     C***                endif
127300010201     C                   eval      cmznrr = *zeros
127400020305     C                   move      §ctrokvt      cmznrr
127500021018     C                   eval      cmzlba = vlrfl1
127600010201     C                   call(e)   'TIS711C'
127700010201     C                   parm                    dscmz
127800010201     C                   parm      *blanks       esito
127900010205     C                   if        %error
128000010205     C                             or cmzerr = '1'
128100010205     C                             or esito  = '1'
128200010205     C                   eval      wrkesito = '3'
128300010205     C                   else
128400010201     C*
128500140117     C* 2° invio EDIVAB
128600010201     C                   reset                   dscmz
128700010201     C                   move      vlrpoi        cmzdst
128800010201     C                   eval      cmzfld = vlrfou
128900010201     C                   eval      cmzmbd = vlrhdl
129000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
129100021009     C***                if        prmfir = *blanks
129200140117     C                   eval      cmzfla = 'EDIVAB0F'
129300140117     C                   eval      cmzmba = 'EDIVAB0F'
129400021009     C***                else
129500021009     C***                eval      cmzfla = prmfir
129600021009     C***                eval      cmzmba = prmfir
129700021009     C***                endif
129800010201     C                   eval      cmznrr = *zeros
129900010201     C                   move      §ctrokvb      cmznrr
130000021018     C                   eval      cmzlba = vlrfl1
130100010201     C                   call(e)   'TIS711C'
130200010201     C                   parm                    dscmz
130300010201     C                   parm      *blanks       esito
130400010201     C                   if        %error
130500010201     C                             or cmzerr = '1'
130600010201     C                             or esito  = '1'
130700010201     C                   eval      wrkesito = '3'
130800010201     C                   endif
130900010205     C                   endif
131000990920     C*
131100000613     C                   ENDSR
131200000613     C***
131300070411
131400141028
131500070411     C     *pssr         BEGSR
131600070411     C*
131700070411     C                   if        %open(tivin00r)
131800070411     C                   close     tivin00r
131900070411     C                   endif
132000140117     C                   if        %open(edivabwr)
132100140117     C                   close     edivabwr
132200070411     C                   endif
132300140117     C                   if        %open(edivatwr)
132400140117     C                   close     edivatwr
132500070411     C                   endif
132600070411     C*
132700070411     C* Effettuo la chiamata al CLLE preposto
132800140117     C                   call(e)   'TITVEVTC'
132900070411     C                   parm                    parccm
133000070411     C                   parm                    parmbr
133100070411     C                   parm      '2'           paropz
133200070411     C*
133300070411     C                   eval      wrkesito = '2'
133400070411     C*
133500070411     C                   seton                                        LR
133600070411     C*
133700070411     C                   ENDSR     '*CANCL'
133800070411     C***
133900160801
134000160801
134100160801     C*--------------------------------------------------------
134200160801     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
134300160801     C*--------------------------------------------------------
134400160801     C     CARTAB        BEGSR
134500160801     C*
134600160801     C* TABELLA '15' - NAZIONI
134700160801     C                   clear                   skNAZISO
134800160801     C                   clear                   skNAZBAR
134900160801     C                   eval      tblKUT = 1
135000160801     C                   eval      tblCOD = '15'
135100160801     C     KEYtabP       setll     tabel00f
135200160801     C     KEYtabP       reade     tabel00f
135300160801     C                   dow       not %eof(tabel00f)
135400160801     C                   if        tblFLG = *blanks
135500160801     C                   movel(p)  tblUNI        ds15
135600160801     C                   add       1             jNAZ
135700160801     C                   eval      skNAZBAR(jNAZ) = tblKEY
135800160801     C                   eval      skNAZISO(jNAZ) = §15COD
135900160801     C                   endif
136000160801     C     KEYtabP       reade     tabel00f
136100160801     C                   enddo
136200160801     C*
136300160801     C                   ENDSR
136400160801     C***
136500070411
136600990910
136700000613     C     *inzsr        BEGSR
136800990910     C*
136900990910     C     *entry        plist
137000990920     C                   parm                    tivlrds
137100990921     C                   parm      wrkesito      esito
137200000724     C                   parm                    prmlit
137300000710     C                   parm                    prmfir
137400000613     C*
137500000830     C* CALCOLA LA DATA CORRENTE
137600140117     C                   time                    wn14             14 0
137700140117     C                   movel     wn14          oracor            6 0          *ORA
137800100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
137900100722     C                   eval      datcor = %dec(%date() : *ISO)
138000160801     C*
138100160801     C* Chiave su TABEL00F - parziale
138200160801     C     KEYtabP       KLIST
138300160801     C                   KFLD                    tblKUT
138400160801     C                   KFLD                    tblCOD
138500000830     C*
138600000613     C                   ENDSR
138700000613     C***
