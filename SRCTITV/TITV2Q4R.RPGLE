000100130612      /TITLE Upload via Internet: traduzione "UPL/DWL"
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300130612     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600130612     Ftitas32c  iF   E           K DISK
000700130612     Ftita430c  iF   E           K DISK
000800130612     Ftivgd00f  O    E             DISK
000900040113     D*------------
001000040113     D* SCHIEDE A PROGRAMMA
001100040113     D*------------
001200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001300010330     D*----------------------------------------------------
001400010330     D* DICHIARAZIOINE VARIABILI DI WRK
001500010330     D*----------------------------------------------------
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900130612     D titasds_w     e ds                  extname(titas00f) prefix(w_) inz
002000130612     D titasds_p     e ds                  extname(titas00f) prefix(p_) inz
002100130612     D trul47ds      E DS
002200010330     D esito           s              1
002300130612     D prmlit          s             10
002400130612     D prmfir          s             10
002500010330     D wrkesito        s                   like(esito)
002600130612     D wStatus         s             15    inz
002700130612     D wPorto          s             13S 2 inz
002800130612     D wDati           s           2048    varying
002900130612     D w_tasRMA        s             15    inz
003000130612     D*
003100130612     D TxtInOut        S           2048
003200130612     D ElencoChar      S            256
003300130612     D TipoElenco      S              1
003400130612     D CharSost        S              1
003500130612     D UpperCase       S              1
003600130612     D ChkNull         S              1
003700130612     D CharNull        S              1
003800130612     D EsitoBon        S              1
003900100326
004000130612
004100030715     D*------------------
004200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004300030715     D*------------------
004400030715     D SkSplitFLD      S             10    DIM(100)
004500030715     D SkSplitCSV      S            256    DIM(100)
004600030715     D CharCSV         S              1
004700030715     D CharTXT         S              1
004800030715     D CharNUM         S              1
004900100318     D CharSOS         S              1
005000030715     D posDa           S              3  0 INZ(*zeros)
005100030715     D posA            S              3  0 INZ(*zeros)
005200030715     D i               s              3  0 INZ(1)
005300030715     D wGiro           s              1  0 INZ(*zeros)
005400030822     D*------------------
005500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005600030822     D*------------------
005700030822     D posDaDft        S              3  0 INZ(*zeros)
005800030822     D posADft         S              3  0 INZ(*zeros)
005900030822     D j               s              3  0 INZ(1)
006000030822     D wGiroDft        s              1  0 INZ(*zeros)
006100100318
006200130612
006300100318     D*------------------
006400100318     D* LINKING A DEFINIZIONI ESTERNE
006500100318     D*------------------
006600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800130612
006900130612
007000130612     D*-------------------
007100130612     D* COSTANTI
007200130612     D*-------------------
007300130612     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007400130612     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007500130612
007600130612
007700130612     D*------------------
007800130612     D* RIDEFINIZIONE DS INPUT
007900130612     D*------------------
008000130612     D inKSU           s              8    inz('00504102')
008100130612     D inTIP           s              2    inz('CF')
008200130612     D inISV           s              2    inz('OE')
008300130612
008400010330
008500010330
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800130612     C*
008900130612     C* Avvio blocco elaborazione TIVGD
009000130612     C                   EXSR      strTRUL47
009100130612     C*
009200130612     C* Se ok a procedere => elaboro
009300130612     C                   if        wProcedi = 'S'
009400130612     C*
009500130612     C* Effettuo elaborazione
009600130612     C                   exsr      opeini
009700130612     C                   exsr      rwvab
009800130612     C*
009900130612     C                   endif
010000130612     C*
010100130612     C* Termino il blocco elaborazione TIVGD
010200130612     C                   EXSR      endTRUL47
010300010601     C*
010400010601     C                   seton                                        lr
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400130612     C                   if        flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012800010601     C*--------------------------------------------------------
012900010601     C     ENDELA        BEGSR
013000040119     C*
013100000616     C*
013200010601     C                   ENDSR
013300010601     C***
013400000613
013500010601
013600010601
013700010330     C*--------------------------------------------------------
013800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013900010330     C*--------------------------------------------------------
014000010605     C     RWVAB         BEGSR
014100010330     C*
014200010330     C                   if        not %open(tivin00r)
014300010330     C                   open      tivin00r
014400010330     C                   endif
014500070823     C*
014600070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014700070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014800130612     C***                exsr      inzvar
014900130612 xxx C***                exsr      impfile
015000010330     C*
015100030822     C                   do        *HIVAL
015200010330     C*
015300030822     C                   read      tivin00r                               70
015400010618     C*
015500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015600010618     C                   if        vinflg = '1'
015700010618     C                   eval      flgOk = '1'
015800010618     C                   endif
015900040510     C*
016000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016100040510     C                   z-add     *zeros        wLenVINDTA        4 0
016200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016600040510     C                   z-add     1             wFlgVINDTA
016700040510     C                   leave
016800040510     C                   endif
016900040510     C                   enddo
017000010618     C*
017100040510     C                   if        vindta > *blanks AND
017200040510     C                             wFlgVINDTA = 1
017300010330     C*
017400010601     C                   if        *in70 = *off and
017500010330     C                             (vinflg = *blanks
017600010330     C                              or vinflg = '0'
017700010330     C                              or vinflg = '2')
017800010330     C*
017900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018000010711     C                   if        vinflg = *blanks or vinflg = '0'
018100010711     C                   clear                   vinmsg
018200010711     C                   endif
018300010601     C*
018400070925     C* Aggiungo un byte a blanks all'inizio d ogni record
018500070925     C                   eval      vindta  = ' ' + %trim(vindta)
018600070925     C*
018700080527     C                   exsr      inzvar
018800100318     C                   exsr      normCSV
018900130612     C                   exsr      impfile
019000010601     C*
019100010601     C                   exsr      PREELA
019200130612     C*
019300130612     C* Scarico il buffer di output
019400030822     C                   if        wGiro = 2
019500130612     C                   exsr      valVGD
019600130612     C                   exsr      wriVGD
019700080513     C                   endif
019800010604     C*
019900130612     C                   if        *in31 = *off
020000010604     C                   eval      vinflg = '1'
020100010604     C                   else
020200010604     C                   eval      vinflg = '2'
020300010604     C                   endif
020400010601     C*
020500010604     C                   endif
020600010604     C*
020700010330     C                   else
020800010330     C                   eval      vinflg = '1'
020900010330     C                   endif
021000010601     C*
021100010601     C  N70              update    tivin000
021200010330     C*
021300030822     C  N70              enddo
021400010601     C*
021500010330     C                   eval      wrkesito = '0'
021600010330     C*
021700010330     C                   if        %open(tivin00r)
021800010330     C                   close     tivin00r
021900010330     C                   endif
022000010601     C*
022100010618     C                   if        flgGiro = '1'
022200010601     C                   exsr      endela
022300010618     C                   endif
022400010330     C*
022500010330     C                   ENDSR
022600010330     C***
022700040119
022800040119
022900040119
023000040119     C*----------------------------------------------------*
023100130612     C*  COMPOSIZIONE BUFFER RECORDS OUTPUT
023200040119     C*----------------------------------------------------*
023300130612     C     VALVGD        BEGSR
023400080513     C*
023500130612     C                   eval      wDati = %editc(tasAAS:'X')+'/'+
023600130612     C                                     %editc(tasLNP:'X')+'/'+
023700130612     C                                     %editc(tasNRS:'X')+'/'+
023800130612     C                                     %editc(tasNSP:'X')+';'+
023900130612     C                                     %editc(w_tasRMN:'X')+';'+
024000130612     C                                      %trim(w_tasRMA)+';'+
024100130612     C                                      %trim(w_tasRSD)+';'+
024200130612     C                                      %trim(w_tasLOD)+';'+
024300130612     C                                     %editc(w_tasPOR:'3')+';'+
024400130612     C                                     %editc(wPorto:'3')+';'+
024500130612     C                                      %trim(wStatus)+';'
024600130612     C*
024700040119     C                   ENDSR
024800130612
024900130612
025000130612
025100130612     C*----------------------------------------------------*
025200130612     C*  SCARICAMENTO BUFFER RECORDS OUTPUT
025300130612     C*----------------------------------------------------*
025400130612     C     WRIVGD        BEGSR
025500130612     C*
025600130612     C* Scarico il buffer di output
025700130612     C                   clear                   tivgd000
025800130612     C                   eval      vgdDTA = wDati
025900130612     C                   eval      vgdTIP = inTIP
026000130612     C                   eval      vgdKSU = inKSU
026100130612     C                   eval      vgdTSC = 'WW'
026200130612     C                   eval      vgdDAT = datcor
026300130612     C                   eval      vgdPGM = 'TITV2Q4R'
026400130612     C                   write     tivgd000
026500130612     C*
026600130612     C                   ENDSR
026700010601
026800010601
026900010601
027000010330     C*----------------------------------------------------*
027100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027200010330     C*----------------------------------------------------*
027300040119     C     INZVAR        BEGSR
027400030822     C*
027500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027600030822     C                   CLEAR                   SkSplitCSV
027700030822     C                   EVAL      i = 1
027800030822     C                   EVAL      posDa = *zeros
027900030822     C                   EVAL      posA  = *zeros
028000080527     C*
028100130612     C                   CLEAR                   titasds_w
028200130612     C                   CLEAR                   tivgd000
028300130612     C                   CLEAR                   wPorto
028400130612     C                   CLEAR                   wStatus
028500130612     C                   CLEAR                   wDati
028600030822     C*
028700030822     C* Reimposto i valori di default
028800030822     C                   EXSR      DEFCAM
028900030822     C*
029000010330     C                   ENDSR
029100010330     C*----------------------------------------------------*
029200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029300010330     C*----------------------------------------------------*
029400010330     C     DEFCAM        BEGSR
029500030822     C*
029600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029700030715     C* e delimitatore testo.
029800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
030000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030100100318     C*
030200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030300100318     C                   EVAL      CharSOS = CharNUM
030400030822     C*
030500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030600030822     C                   EVAL      posDaDft = 1
030700030822     C                   EVAL      posADft  = 0
030800030822     C                   EVAL      wGiroDft = 0
030900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031000030822     C                             posDaDft > 0
031100030822     C*
031200030822     C* Gestisco il 1° giro
031300030822     C                   IF        wGiroDft = 0
031400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031600030822     C* Incremento il contatore dei "giri"
031700030822     C                   EVAL      wGiroDft = 1
031800030822     C                   ELSE
031900030822     C                   EVAL      posDaDft = posADft
032000030822     C                   ENDIF
032100030822     C* Eseguo lo scan x trovare la fine del campo corrente
032200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032300030822     C*
032400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
032600030822     C                   IF        posDaDft > 0 AND
032700030822     C                             posADft  > 0
032800130612     C* CCM
032900030822     C                   IF        %subst(
033000030822     C                             %subst(vlrppt:posDaDft+1:
033100030822     C                             posADft-posDaDft-1):1:3)
033200030822     C                             = 'CCM'
033300030822     C                   EVAL      PiStr=%trim(%subst(
033400030822     C                             %subst(vlrppt:posDaDft+1:
033500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033600030822     C                   EXSR      CHKNUM
033700030822     C                   IF        PiInt=*on
033800130612     C                   Z-ADD     PiVal         p_tasCCM
033900030822     C                   ENDIF
034000030822     C                   ENDIF
034100030822     C* LNP
034200030822     C                   IF        %subst(
034300030822     C                             %subst(vlrppt:posDaDft+1:
034400030822     C                             posADft-posDaDft-1):1:3)
034500030822     C                             = 'LNP'
034600030822     C                   EVAL      PiStr=%trim(%subst(
034700030822     C                             %subst(vlrppt:posDaDft+1:
034800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034900030822     C                   EXSR      CHKNUM
035000030822     C                   IF        PiInt=*on
035100130612     C                   Z-ADD     PiVal         p_tasLNP
035200030822     C                   ENDIF
035300030822     C                   ENDIF
035400030822     C* NRS
035500030822     C                   IF        %subst(
035600030822     C                             %subst(vlrppt:posDaDft+1:
035700030822     C                             posADft-posDaDft-1):1:3)
035800030822     C                             = 'NRS'
035900030822     C                   EVAL      PiStr=%trim(%subst(
036000030822     C                             %subst(vlrppt:posDaDft+1:
036100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036200030822     C                   EXSR      CHKNUM
036300030822     C                   IF        PiInt=*on
036400130612     C                   Z-ADD     PiVal         p_tasNRS
036500030822     C                   ENDIF
036600030822     C                   ENDIF
036700030822     C* CTR
036800030822     C                   IF        %subst(
036900030822     C                             %subst(vlrppt:posDaDft+1:
037000030822     C                             posADft-posDaDft-1):1:3)
037100030822     C                             = 'CTR'
037200030822     C                   EVAL      PiStr=%trim(%subst(
037300030822     C                             %subst(vlrppt:posDaDft+1:
037400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037500030822     C                   EXSR      CHKNUM
037600030822     C                   IF        PiInt=*on
037700130612     C                   Z-ADD     PiVal         p_tasCTR
037800030822     C                   ENDIF
037900030822     C                   ENDIF
038000130612     C* TBL
038100030822     C                   IF        %subst(
038200030822     C                             %subst(vlrppt:posDaDft+1:
038300030822     C                             posADft-posDaDft-1):1:3)
038400130612     C                             = 'TBL'
038500130612     C                   EVAL      p_tasTBL=%trim(%subst(
038600030822     C                             %subst(vlrppt:posDaDft+1:
038700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038800030822     C                   ENDIF
038900030822     C* TSP
039000030822     C                   IF        %subst(
039100030822     C                             %subst(vlrppt:posDaDft+1:
039200030822     C                             posADft-posDaDft-1):1:3)
039300030822     C                             = 'TSP'
039400130612     C                   EVAL      p_tasTSP=%trim(%subst(
039500030822     C                             %subst(vlrppt:posDaDft+1:
039600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039700030822     C                   ENDIF
039800030822     C* CTM
039900030822     C                   IF        %subst(
040000030822     C                             %subst(vlrppt:posDaDft+1:
040100030822     C                             posADft-posDaDft-1):1:3)
040200030822     C                             = 'CTM'
040300130612     C                   EVAL      p_tasCTM=%trim(%subst(
040400030822     C                             %subst(vlrppt:posDaDft+1:
040500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040600030822     C                   ENDIF
040700030822     C* FFD
040800030822     C                   IF        %subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):1:3)
041100030822     C                             = 'FFD'
041200130612     C                   EVAL      p_tasFFD=%trim(%subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500030822     C                   ENDIF
041600130612     C* inKSU
041700130612     C                   IF        %subst(
041800130612     C                             %subst(vlrppt:posDaDft+1:
041900130612     C                             posADft-posDaDft-1):1:3)
042000130612     C                             = 'KSU'
042100130612     C                   EVAL      inKSU=%trim(%subst(
042200130612     C                             %subst(vlrppt:posDaDft+1:
042300130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400130612     C                   ENDIF
042500130612     C* inTIP
042600130612     C                   IF        %subst(
042700130612     C                             %subst(vlrppt:posDaDft+1:
042800130612     C                             posADft-posDaDft-1):1:3)
042900130612     C                             = 'TIP'
043000130612     C                   EVAL      inTIP=%trim(%subst(
043100130612     C                             %subst(vlrppt:posDaDft+1:
043200130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043300130612     C                   ENDIF
043400130612     C* inISV
043500130612     C                   IF        %subst(
043600130612     C                             %subst(vlrppt:posDaDft+1:
043700130612     C                             posADft-posDaDft-1):1:3)
043800130612     C                             = 'ISV'
043900130612     C                   EVAL      inISV=%trim(%subst(
044000130612     C                             %subst(vlrppt:posDaDft+1:
044100130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044200130612     C                   ENDIF
044300030822     C* ...
044400030822     C                   ENDIF
044500030822     C                   ENDDO
044600020204     C*
044700010330     C                   ENDSR
044800010607     C*----------------------------------------------------*
044900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
045000010607     C*----------------------------------------------------*
045100130612     C     IMPFILE       BEGSR
045200010607     C*
045300130612     C                   SETOFF                                       31
045400010607     C*
045500030715     C**********
045600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
045700030715     C**********
045800030715     C                   IF        wGiro = *zeros
045900040113     C*
046000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
046100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
046200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
046300080429     C                             %trim(intcol(5))<>*blanks
046400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
046500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
046600080429     C                                      %trim(intcol(5))
046700070823     C     ';':CharCSV   XLATE     vindta        vindta
046800040510     C                   ENDIF
046900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
047000031201     C     minu:maiu     XLATE     vindta        vindta
047100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047200030715     C                   DOW       posDa <= %len(%trim(vindta))
047300030715     C*
047400030715     C* Gestisco il 1° campo
047500030715     C                   IF        i = 1
047600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
047700030715     C                   EVAL      posDa = 1
047800030715     C* Eseguo lo scan x trovare la fine del primo campo
047900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048000030715     C* A questo punto "estraggo" il campo corrente
048100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048200030715     C                                                    (posA-posDa))
048300030715     C* X i campi successivi al 1°
048400030715     C                   ELSE
048500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
048600030715     C                   EVAL      posDa = posA + 1
048700030715     C* Eseguo lo scan x trovare la fine del campo corrente
048800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048900030715     C* Gestisco l'ultimo campo
049000030715     C                   IF        posA = *zeros
049100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
049200030715     C* A questo punto "estraggo" il campo corrente
049300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
049400030715     C                                                    (posA-posDa))
049500030715     C                   LEAVE
049600030715     C                   ELSE
049700030715     C* A questo punto "estraggo" il campo corrente
049800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
049900030715     C                                                    (posA-posDa))
050000030715     C                   ENDIF
050100030715     C                   ENDIF
050200030715     C* Incremento il contatore d campo
050300030715     C                   EVAL      i = i +1
050400030715     C                   ENDDO
050500030715     C*
050600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
050700030715     C* delimitazione testo
050800030715     C                   EVAL      i = 1
050900030715     C                   DOW       i < %elem(SkSplitFLD)
051000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
051100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
051200030715     C                   EVAL      i = i + 1
051300030715     C                   ENDDO
051400030715     C                   EVAL      wGiro = 1
051500030715     C                   ELSE
051600030715     C                   EVAL      wGiro = 2
051700031204     C**********
051800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
051900031204     C* d separatore campo
052000031204     C**********
052100031204     C                   EVAL      vindta = %trim(vindta)
052200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
052300031204     C                   EVAL      vindta = %subst(vindta:2)
052400031204     C                   ENDDO
052500031204     C*
052600031204     C                   Z-ADD     *zeros        lunghInput        4 0
052700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
052800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
052900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
053000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
053100031204     C                   ENDDO
053200030715     C**********
053300030715     C* Effettuo lo split del campo dati d input
053400030715     C**********
053500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
053600030715     C                   DOW       posDa <= %len(%trim(vindta))
053700030715     C*
053800030715     C* Gestisco il 1° campo
053900030715     C                   IF        i = 1
054000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
054100030715     C                   EVAL      posDa = 1
054200030715     C* Eseguo lo scan x trovare la fine del primo campo
054300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
054400030715     C* A questo punto "estraggo" il campo corrente
054500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
054600030715     C                                                    (posA-posDa))
054700030715     C* X i campi successivi al 1°
054800030715     C                   ELSE
054900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
055000030715     C                   EVAL      posDa = posA + 1
055100031204     C* Verifico che nn vi sia il campo nn valorizzato
055200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
055300031204     C* Se campo nn valorizzato skippo al prossimo
055400031204     C                   EVAL      SkSplitCSV(i) = *blanks
055500031204     C                   EVAL      posA  = posA + 1
055600031204     C                   ELSE
055700030715     C* Eseguo lo scan x trovare la fine del campo corrente
055800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
055900030715     C* Gestisco l'ultimo campo
056000030715     C                   IF        posA = *zeros
056100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
056200030715     C* A questo punto "estraggo" il campo corrente
056300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
056400030715     C                                                    (posA-posDa))
056500030715     C                   LEAVE
056600030715     C                   ELSE
056700030715     C* A questo punto "estraggo" il campo corrente
056800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
056900030715     C                                                    (posA-posDa))
057000030715     C                   ENDIF
057100031204     C                   ENDIF
057200030715     C                   ENDIF
057300030715     C* Incremento il contatore d campo
057400030715     C                   EVAL      i = i +1
057500030715     C                   ENDDO
057600030715     C*
057700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
057800030715     C* delimitazione testo
057900030715     C                   EVAL      i = 1
058000030715     C                   DOW       i < %elem(SkSplitCSV)
058100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
058200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
058300030715     C                   EVAL      i = i + 1
058400030715     C                   ENDDO
058500100326     C*
058600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
058700030715     C                   EVAL      i = 1
058800030715     C                   DOW       i < %elem(SkSplitFLD)
058900030715     C*
059000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
059100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
059200030715     C                             SkSplitCSV(i) <> *blanks
059300070823     C*
059400130612 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
059500110421     C*
059600130612     C*** RMN
059700130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMN'
059800130612     C                   EVAL      PiStr=SkSplitCSV(i)
059900130612     C                   EXSR      CHKNUM
060000130612     C                   IF        PiInt=*on
060100130612     C                   Z-ADD     PiVal         w_tasRMN
060200130612     C                   ELSE
060300130612     C                   SETON                                        31
060400130612     C                   EVAL      w_tasRMN = *zeros
060500130612     C                   EVAL      vinmsg = %trimr(vinmsg)
060600130612     C                             + ' ' + 'INRMN'
060700130612     C                   ENDIF
060800130612     C                   ENDIF
060900130612     C*** AAS
061000130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INAAS'
061100130612     C                   EVAL      PiStr=SkSplitCSV(i)
061200130612     C                   EXSR      CHKNUM
061300130612     C                   IF        PiInt=*on
061400130612     C                   Z-ADD     PiVal         wDATA             8 0
061500130612     C                   MOVEL     wDATA         w_tasAAS
061600130612     C                   ELSE
061700130612     C                   SETON                                        31
061800130612     C                   EVAL      w_tasAAS = *zeros
061900130612     C                   EVAL      vinmsg = %trimr(vinmsg)
062000130612     C                             + ' ' + 'INAAS'
062100130612     C                   ENDIF
062200130612     C                   ENDIF
062300130612     C*** POR
062400130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INPOR'
062500130612     C*
062600130612     C* Effettuo la chiamata al *pgm d "bonifica" x il campo 'W_INPOR'
062700130612     C                   EVAL      TxtInOut   = SkSplitCSV(i)
062800130612     C                   EVAL      ElencoChar = '1234567890,.'
062900130612     C                   EVAL      EsitoBon   = *blanks
063000130612     C*
063100130612     C                   CALL      'XCHKCHAR'
063200130612     C                   PARM                    TxtInOut
063300130612     C                   PARM                    ElencoChar
063400130612     C                   PARM                    TipoElenco
063500130612     C                   PARM                    CharSost
063600130612     C                   PARM                    UpperCase
063700130612     C                   PARM                    ChkNull
063800130612     C                   PARM                    CharNull
063900130612     C                   PARM                    EsitoBon
064000130612     C*
064100130612     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
064200130612     C                   IF        EsitoBon = '1'
064300130612     C                   EVAL      SkSplitCSV(i) = TxtInOut
064400130612     C                   ENDIF
064500130612     C*
064600130612     C                   EVAL      PiStr=SkSplitCSV(i)
064700130612     C                   EXSR      CHKNUM
064800130612     C                   IF        PiNum=*on
064900130612     C                   Z-ADD     PiVal         w_tasPOR
065000130612     C                   ELSE
065100130612     C                   SETON                                        31
065200130612     C                   EVAL      w_tasPOR = *zeros
065300130612     C                   EVAL      vinmsg = %trimr(vinmsg)
065400130612     C                             + ' ' + 'INPOR'
065500130612     C                   ENDIF
065600130612     C                   ENDIF
065700130612     C*** RSD
065800130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRSD'
065900130612     C                   EVAL      w_tasRSD = SkSplitCSV(i)
066000110421     C                   ENDIF
066100130612     C*** LOD
066200130612     C                   IF        %trim(SkSplitFLD(i)) = 'INLOD'
066300130612     C                   EVAL      w_tasLOD = SkSplitCSV(i)
066400130612     C                   ENDIF
066500130612     C*** RMA
066600130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMA'
066700130612     C                   EVAL      w_tasRMA = SkSplitCSV(i)
066800130612     C                   ENDIF
066900070925     C*
067000071029     C***  <===  -----------------------------------------
067100040510     C*
067200030715     C                   ENDIF
067300030715     C                   EVAL      i = i + 1
067400030715     C                   ENDDO
067500070925     C*
067600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
067700130612     C*
067800130612     C* Aggancio la bolla in sede
067900130612     C                   movel     'N'           wOK               1
068000130612     C     KEYtas32C     setll     titas32c
068100130612     C                   if        %equal(titas32c)
068200130612     C     KEYtas32C     reade     titas32c
068300130612     C                   dow       not %eof(titas32c)
068400130612     C                   movel     'S'           wOK
068500130612     C*
068600130612     C* Se richiesti applico i filtri
068700130612     C*
068800130612     C                   if        w_tasAAS > *zeros
068900130612     C                   if        w_tasAAS = tasAAS
069000130612     C                   else
069100130612     C                   movel     'N'           wOK
069200130612     C                   endif
069300130612     C                   endif
069400130612     C*
069500130612     C                   if        p_tasCCM > *zeros
069600130612     C                   if        p_tasCCM = tasCCM
069700130612     C                   else
069800130612     C                   movel     'N'           wOK
069900130612     C                   endif
070000130612     C                   endif
070100130612     C*
070200130612     C                   if        p_tasLNP > *zeros
070300130612     C                   if        p_tasLNP = tasLNP
070400130612     C                   else
070500130612     C                   movel     'N'           wOK
070600130612     C                   endif
070700130612     C                   endif
070800130612     C*
070900130612     C                   if        p_tasNRS > *zeros
071000130612     C                   if        p_tasNRS = tasNRS
071100130612     C                   else
071200130612     C                   movel     'N'           wOK
071300130612     C                   endif
071400130612     C                   endif
071500130612     C*
071600130612     C                   if        p_tasCTR > *zeros
071700130612     C                   if        p_tasCTR = tasCTR
071800130612     C                   else
071900130612     C                   movel     'N'           wOK
072000130612     C                   endif
072100130612     C                   endif
072200130612     C*
072300130612     C                   if        p_tasTBL <> *blanks
072400130612     C                   if        p_tasTBL  = tasTBL
072500130612     C                   else
072600130612     C                   movel     'N'           wOK
072700130612     C                   endif
072800130612     C                   endif
072900130612     C*
073000130612     C                   if        p_tasTSP <> *blanks
073100130612     C                   if        p_tasTSP  = tasTSP
073200130612     C                   else
073300130612     C                   movel     'N'           wOK
073400130612     C                   endif
073500130612     C                   endif
073600130612     C*
073700130612     C                   if        p_tasCTM <> *blanks
073800130612     C                   if        p_tasCTM  = tasCTM
073900130612     C                   else
074000130612     C                   movel     'N'           wOK
074100130612     C                   endif
074200130612     C                   endif
074300130612     C*
074400130612     C                   if        p_tasFFD <> *blanks
074500130612     C                   if        p_tasFFD  = tasFFD
074600130612     C                   else
074700130612     C                   movel     'N'           wOK
074800130612     C                   endif
074900130612     C                   endif
075000130612     C*
075100130612     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
075200130612     C                   clear                   ta4NOT
075300130612     C                   eval      ta4TRC = 'A'
075400130612     C     KEYta430      chain     tita430c
075500130612     C                   if        %found(tita430c)
075600130612     C*
075700130612     C                   if        w_tasRMA <> *blanks
075800130612     C                   if        w_tasRMA  = ta4NOT
075900130612     C                   else
076000130612     C                   movel     'N'           wOK
076100130612     C                   endif
076200130612     C                   endif
076300130612     C*
076400130612     C                   endif
076500130612     C*
076600130612     C     KEYtas32C     reade     titas32c
076700130612     C                   enddo
076800130612     C                   endif
076900070925     C*
077000071029     C***  <===  -----------------------------------------
077100070925     C*
077200040510     C*
077300130612     C* Considerazioni finali sul confronto fattura (solo su PORTO)
077400130612     C                   if        *in31
077500130612     C                   eval      wStatus = 'ERR_INPUT'
077600130612     C                   else
077700130612     C                   if        wOK <> 'S'
077800130612     C                   eval      wStatus = 'MANCA BRT'
077900130612     C                   else
078000130612     C                   z-add     tasPOR        wPorto
078100130612     C                   if        wPorto  = w_tasPOR
078200130612     C                   eval      wStatus = 'CONFORME'
078300130612     C                   else
078400130612     C                   eval      wStatus = 'DIFFERENZA'
078500130612     C                   endif
078600130612     C                   endif
078700130612     C                   endif
078800130612     C*
078900130612     C                   ENDIF
079000020204     C*
079100010607     C                   ENDSR
079200010607     C*----------------------------------------------------*
079300010330
079400100318
079500100318
079600100318     C*----------------------------------------------------*
079700100318     C*  CONTROLLO NUMERICITA' CAMPI
079800100318     C*----------------------------------------------------*
079900100318     C     CHKNUM        BEGSR
080000100318     C*
080100100318     C                   IF        PiDecChr = *blanks
080200100318     C                   EVAL      PiDecChr = CharNUM
080300100318     C                   ENDIF
080400100318     C*
080500100318     C                   callp     UBISNUM_Check(PiStr
080600100318     C                                          :PiDecChr
080700100318     C                                          :PiVal
080800100318     C                                          :PiNum
080900100318     C                                          :PiInt)
081000100318     C*
081100100318     C                   ENDSR
081200010601     C***
081300010601
081400010601
081500010601
081600010601      /TITLE Invio dei dati al punto operativo.
081700010601     C     opeini        BEGSR
081800010601     C*
081900010601     C* Inizializzo flag e contatori operativi
082000010601     C                   movel     '0'           flgGiro           1
082100010601     C                   movel     '0'           flgMulti          1
082200010601     C                   movel     '1'           flgStato          1
082300010615     C                   movel     '0'           flgOk             1
082400010601     C                   z-add     *zeros        cntNonEl         10 0
082500010601     C                   z-add     *zeros        invfil            3 0
082600130612     C*
082700130612     C* Scrivo la riga di intestazione colonne del file di output
082800130612     C                   eval      wDati = 'SPED BRT;'+
082900130612     C                                     'RIF_N MITT;'+
083000130612     C                                     'RIF_A MITT;'+
083100130612     C                                     'DESTINATARIO;'+
083200130612     C                                     'LOCALITA;'+
083300130612     C                                     'FATT CLIENTE;'+
083400130612     C                                     'FATT BRT;'+
083500130612     C                                     'STATO;'
083600130612     C                   exsr      wriVGD
083700010601     C*
083800010601     C                   ENDSR
083900010601     C***
084000100318
084100100318
084200100318
084300100318     C     normCSV       BEGSR
084400100318     C*
084500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
084600100318     C* separatori campi
084700100318     C                   if        CharTXT <> *blanks
084800100318     C                   z-add     *zeros        wLenVINDTA        4 0
084900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
085000100318     C                   setoff                                       80
085100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
085200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
085300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
085400100318     C                   if        not *in80
085500100318     C                   seton                                        80
085600100318     C                   else
085700100318     C                   setoff                                       80
085800100318     C                   endif
085900100318     C                   endif
086000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
086100100318     C                             and *in80 = *on
086200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
086300100318     C                   endif
086400100318     C                   enddo
086500100318     C                   endif
086600100318     C*
086700100318     C                   ENDSR
086800100318     C***
086900130612
087000130612
087100130612
087200130612     C     strTRUL47     BEGSR
087300130612     C*
087400130612     C                   movel     'N'           wProcedi          1
087500130612     C*
087600130612     C* Avvio il blocco elaborazione TIVGD x tipo file in questione
087700130612     C                   clear                   trul47ds
087800130612     C                   eval      d47opz  = 'I'
087900130612     C                   eval      d47tip  = inTIP
088000130612     C                   eval      d47lck  = 'N'
088100130612     C                   eval      d47chkj = 'S'
088200130612     C                   eval      d47pgm  = 'TITV2Q4R'
088300130612     C                   call      'TRUL47R'
088400130612     C                   parm                    trul47ds
088500130612     C*
088600130612     C* Se elaborazione consentita => proseguo
088700130612     C                   if        d47sts <> 'A'
088800130612     C                   movel     'S'           wProcedi
088900130612     C                   endif
089000130612     C*
089100130612     C                   ENDSR
089200130612
089300130612
089400130612
089500130612     C     endTRUL47     BEGSR
089600130612     C*
089700130612     C                   if        wProcedi = 'S'
089800130612     C*
089900130612     C* Elimino il blocco elaborazione TIVGD x tipo file in questione
090000130612     C                   clear                   trul47ds
090100130612     C                   eval      d47opz  = 'F'
090200130612     C                   eval      d47tip  = inTIP
090300130612     C                   call      'TRUL47R'
090400130612     C                   parm                    trul47ds
090500130612     C*
090600130612     C                   endif
090700130612     C*
090800130612     C                   ENDSR
090900010330
091000100326
091100010330
091200000613     C     *inzsr        BEGSR
091300990910     C*
091400990910     C     *entry        plist
091500130612     C                   parm                    tivlrds
091600990921     C                   parm      wrkesito      esito
091700000724     C                   parm                    prmlit
091800000710     C                   parm                    prmfir
091900010330     C*
092000010330     C* CALCOLA LA DATA CORRENTE
092100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
092200100318     C                   eval      datcor = %dec(%date() : *ISO)
092300130612     C*
092400130612     C* Definizione chiavi
092500130612     C*
092600130612     C* Chiave su TITAS32C - Completa
092700130612     C     KEYtas32C     KLIST
092800130612     C                   KFLD                    w_tasRMN
092900130612     C                   KFLD                    p_tasCCM
093000130612     C*
093100130612     C* Chiave su TITA430C - Completa
093200130612     C     KEYta430      KLIST
093300130612     C                   KFLD                    tasAAS
093400130612     C                   KFLD                    tasLNP
093500130612     C                   KFLD                    tasNRS
093600130612     C                   KFLD                    tasNSP
093700130612     C                   KFLD                    ta4TRC
093800000613     C*
093900000613     C                   ENDSR
094000000613     C***
094100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
094200130612INRMN;W_INAAS;INRSD;INLOD;W_INPOR;INRMA
094300110421
094400110315
094500110315
094600100318
