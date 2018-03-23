000100130612      /TITLE Upload via Internet: traduzione "UPL/DWL"
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300130612     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600130612     Ftitas32c  iF   E           K DISK
000700130612     Ftita430c  iF   E           K DISK
000800130612     Ftivgd00f  O    E             DISK
000900130612     Ftis7prgf  uf   E             DISK    RENAME(tis7prgf:tis7prg0)
001000130612     F                                     PREFIX(f_)
001100130612     F                                     USROPN
001200040113     D*------------
001300040113     D* SCHIEDE A PROGRAMMA
001400040113     D*------------
001500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D psds           sds
002000010330     D  procname         *PROC
002100010330     D tivlrds       e ds                  extname(tivlr00f)
002200130612     D titasds_w     e ds                  extname(titas00f) prefix(w_) inz
002300130612     D titasds_p     e ds                  extname(titas00f) prefix(p_) inz
002400130612     D trul47ds      E DS
002500010330     D esito           s              1
002600130612     D prmlit          s             10
002700130612     D prmfir          s             10
002800010330     D wrkesito        s                   like(esito)
002900130612     D wStatus         s             15    inz
003000130612     D wPorto          s             13S 2 inz
003100130612     D wDati           s           2048    varying
003200130612     D w_tasRMA        s             15    inz
003300130612     D*
003400130612     D TxtInOut        S           2048
003500130612     D ElencoChar      S            256
003600130612     D TipoElenco      S              1
003700130612     D CharSost        S              1
003800130612     D UpperCase       S              1
003900130612     D ChkNull         S              1
004000130612     D CharNull        S              1
004100130612     D EsitoBon        S              1
004200100326
004300130612
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200100318     D CharSOS         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700030822     D*------------------
005800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005900030822     D*------------------
006000030822     D posDaDft        S              3  0 INZ(*zeros)
006100030822     D posADft         S              3  0 INZ(*zeros)
006200030822     D j               s              3  0 INZ(1)
006300030822     D wGiroDft        s              1  0 INZ(*zeros)
006400100318
006500130612
006600100318     D*------------------
006700100318     D* LINKING A DEFINIZIONI ESTERNE
006800100318     D*------------------
006900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007100130612
007200130612
007300130612     D*-------------------
007400130612     D* COSTANTI
007500130612     D*-------------------
007600130612     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007700130612     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007800130612
007900130612
008000130612     D*------------------
008100130612     D* RIDEFINIZIONE DS INPUT
008200130612     D*------------------
008300130612     D inKSU           s              8    inz('00504102')
008400130612     D inTIP           s              2    inz('CF')
008500130612     D inISV           s              2    inz('OE')
008600130612
008700010330
008800010330
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100130612     C*
009200130612     C* Avvio blocco elaborazione TIVGD
009300130612     C                   EXSR      strTRUL47
009400130612     C*
009500130612     C* Se ok a procedere => elaboro
009600130612     C                   if        wProcedi = 'S'
009700130612     C*
009800130612     C* Effettuo elaborazione
009900130612     C                   exsr      opeini
010000130612     C                   exsr      rwvab
010100130612     C*
010200130612     C                   endif
010300130612     C*
010400130612     C* Termino il blocco elaborazione TIVGD
010500130612     C                   EXSR      endTRUL47
010600010601     C*
010700010601     C                   seton                                        lr
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700130612     C                   if        flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100010601     C*
012200010601     C                   endif
012300010601     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601
012900010601     C*--------------------------------------------------------
013000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013100010601     C*--------------------------------------------------------
013200010601     C     ENDELA        BEGSR
013300040119     C*
013400000616     C*
013500010601     C                   ENDSR
013600010601     C***
013700000613
013800010601
013900010601
014000010330     C*--------------------------------------------------------
014100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014200010330     C*--------------------------------------------------------
014300010605     C     RWVAB         BEGSR
014400010330     C*
014500010330     C                   if        not %open(tivin00r)
014600010330     C                   open      tivin00r
014700010330     C                   endif
014800070823     C*
014900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
015000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015100130612     C***                exsr      inzvar
015200130612 xxx C***                exsr      impfile
015300010330     C*
015400030822     C                   do        *HIVAL
015500010330     C*
015600030822     C                   read      tivin00r                               70
015700010618     C*
015800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015900010618     C                   if        vinflg = '1'
016000010618     C                   eval      flgOk = '1'
016100010618     C                   endif
016200040510     C*
016300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016400040510     C                   z-add     *zeros        wLenVINDTA        4 0
016500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016900040510     C                   z-add     1             wFlgVINDTA
017000040510     C                   leave
017100040510     C                   endif
017200040510     C                   enddo
017300010618     C*
017400040510     C                   if        vindta > *blanks AND
017500040510     C                             wFlgVINDTA = 1
017600010330     C*
017700010601     C                   if        *in70 = *off and
017800010330     C                             (vinflg = *blanks
017900010330     C                              or vinflg = '0'
018000010330     C                              or vinflg = '2')
018100010330     C*
018200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018300010711     C                   if        vinflg = *blanks or vinflg = '0'
018400010711     C                   clear                   vinmsg
018500010711     C                   endif
018600010601     C*
018700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
018800070925     C                   eval      vindta  = ' ' + %trim(vindta)
018900070925     C*
019000080527     C                   exsr      inzvar
019100100318     C                   exsr      normCSV
019200130612     C                   exsr      impfile
019300010601     C*
019400010601     C                   exsr      PREELA
019500130612     C*
019600130612     C* Scarico il buffer di output
019700030822     C                   if        wGiro = 2
019800130612     C                   exsr      valVGD
019900130612     C                   exsr      wriVGD
020000080513     C                   endif
020100010604     C*
020200130612     C                   if        *in31 = *off
020300010604     C                   eval      vinflg = '1'
020400010604     C                   else
020500010604     C                   eval      vinflg = '2'
020600010604     C                   endif
020700010601     C*
020800010604     C                   endif
020900010604     C*
021000010330     C                   else
021100010330     C                   eval      vinflg = '1'
021200010330     C                   endif
021300010601     C*
021400010601     C  N70              update    tivin000
021500010330     C*
021600030822     C  N70              enddo
021700010601     C*
021800010330     C                   eval      wrkesito = '0'
021900010330     C*
022000010330     C                   if        %open(tivin00r)
022100010330     C                   close     tivin00r
022200010330     C                   endif
022300010601     C*
022400010618     C                   if        flgGiro = '1'
022500010601     C                   exsr      endela
022600010618     C                   endif
022700010330     C*
022800010330     C                   ENDSR
022900010330     C***
023000040119
023100040119
023200040119
023300040119     C*----------------------------------------------------*
023400130612     C*  COMPOSIZIONE BUFFER RECORDS OUTPUT
023500040119     C*----------------------------------------------------*
023600130612     C     VALVGD        BEGSR
023700080513     C*
023800130612     C                   eval      wDati = %editc(tasAAS:'X')+'/'+
023900130612     C                                     %editc(tasLNP:'X')+'/'+
024000130612     C                                     %editc(tasNRS:'X')+'/'+
024100130612     C                                     %editc(tasNSP:'X')+';'+
024200130612     C                                     %editc(w_tasRMN:'X')+';'+
024300130612     C                                      %trim(w_tasRMA)+';'+
024400130612     C                                      %trim(w_tasRSD)+';'+
024500130612     C                                      %trim(w_tasLOD)+';'+
024600130612     C                                     %editc(w_tasPOR:'3')+';'+
024700130612     C                                     %editc(wPorto:'3')+';'+
024800130612     C                                      %trim(wStatus)+';'
024900130612     C*
025000040119     C                   ENDSR
025100130612
025200130612
025300130612
025400130612     C*----------------------------------------------------*
025500130612     C*  SCARICAMENTO BUFFER RECORDS OUTPUT
025600130612     C*----------------------------------------------------*
025700130612     C     WRIVGD        BEGSR
025800130612     C*
025900130612     C* Scarico il buffer di output
026000130612     C                   clear                   tivgd000
026100130612     C                   eval      vgdDTA = wDati
026200130612     C                   eval      vgdTIP = inTIP
026300130612     C                   eval      vgdKSU = inKSU
026400130612     C                   eval      vgdTSC = 'WW'
026500130612     C                   eval      vgdDAT = datcor
026600130612     C                   eval      vgdPGM = 'TITV2Q4R'
026700130612     C                   write     tivgd000
026800130612     C*
026900130612     C                   ENDSR
027000010601
027100010601
027200010601
027300010330     C*----------------------------------------------------*
027400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500010330     C*----------------------------------------------------*
027600040119     C     INZVAR        BEGSR
027700030822     C*
027800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027900030822     C                   CLEAR                   SkSplitCSV
028000030822     C                   EVAL      i = 1
028100030822     C                   EVAL      posDa = *zeros
028200030822     C                   EVAL      posA  = *zeros
028300080527     C*
028400130612     C                   CLEAR                   titasds_w
028500130612     C                   CLEAR                   tivgd000
028600130612     C                   CLEAR                   wPorto
028700130612     C                   CLEAR                   wStatus
028800130612     C                   CLEAR                   wDati
028900030822     C*
029000030822     C* Reimposto i valori di default
029100030822     C                   EXSR      DEFCAM
029200030822     C*
029300010330     C                   ENDSR
029400010330     C*----------------------------------------------------*
029500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029600010330     C*----------------------------------------------------*
029700010330     C     DEFCAM        BEGSR
029800030822     C*
029900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030000030715     C* e delimitatore testo.
030100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
030200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
030300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030400100318     C*
030500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030600100318     C                   EVAL      CharSOS = CharNUM
030700030822     C*
030800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030900030822     C                   EVAL      posDaDft = 1
031000030822     C                   EVAL      posADft  = 0
031100030822     C                   EVAL      wGiroDft = 0
031200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031300030822     C                             posDaDft > 0
031400030822     C*
031500030822     C* Gestisco il 1° giro
031600030822     C                   IF        wGiroDft = 0
031700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031900030822     C* Incremento il contatore dei "giri"
032000030822     C                   EVAL      wGiroDft = 1
032100030822     C                   ELSE
032200030822     C                   EVAL      posDaDft = posADft
032300030822     C                   ENDIF
032400030822     C* Eseguo lo scan x trovare la fine del campo corrente
032500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032600030822     C*
032700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
032900030822     C                   IF        posDaDft > 0 AND
033000030822     C                             posADft  > 0
033100130612     C* CCM
033200030822     C                   IF        %subst(
033300030822     C                             %subst(vlrppt:posDaDft+1:
033400030822     C                             posADft-posDaDft-1):1:3)
033500030822     C                             = 'CCM'
033600030822     C                   EVAL      PiStr=%trim(%subst(
033700030822     C                             %subst(vlrppt:posDaDft+1:
033800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033900030822     C                   EXSR      CHKNUM
034000030822     C                   IF        PiInt=*on
034100130612     C                   Z-ADD     PiVal         p_tasCCM
034200030822     C                   ENDIF
034300030822     C                   ENDIF
034400030822     C* LNP
034500030822     C                   IF        %subst(
034600030822     C                             %subst(vlrppt:posDaDft+1:
034700030822     C                             posADft-posDaDft-1):1:3)
034800030822     C                             = 'LNP'
034900030822     C                   EVAL      PiStr=%trim(%subst(
035000030822     C                             %subst(vlrppt:posDaDft+1:
035100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035200030822     C                   EXSR      CHKNUM
035300030822     C                   IF        PiInt=*on
035400130612     C                   Z-ADD     PiVal         p_tasLNP
035500030822     C                   ENDIF
035600030822     C                   ENDIF
035700030822     C* NRS
035800030822     C                   IF        %subst(
035900030822     C                             %subst(vlrppt:posDaDft+1:
036000030822     C                             posADft-posDaDft-1):1:3)
036100030822     C                             = 'NRS'
036200030822     C                   EVAL      PiStr=%trim(%subst(
036300030822     C                             %subst(vlrppt:posDaDft+1:
036400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500030822     C                   EXSR      CHKNUM
036600030822     C                   IF        PiInt=*on
036700130612     C                   Z-ADD     PiVal         p_tasNRS
036800030822     C                   ENDIF
036900030822     C                   ENDIF
037000030822     C* CTR
037100030822     C                   IF        %subst(
037200030822     C                             %subst(vlrppt:posDaDft+1:
037300030822     C                             posADft-posDaDft-1):1:3)
037400030822     C                             = 'CTR'
037500030822     C                   EVAL      PiStr=%trim(%subst(
037600030822     C                             %subst(vlrppt:posDaDft+1:
037700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800030822     C                   EXSR      CHKNUM
037900030822     C                   IF        PiInt=*on
038000130612     C                   Z-ADD     PiVal         p_tasCTR
038100030822     C                   ENDIF
038200030822     C                   ENDIF
038300130612     C* TBL
038400030822     C                   IF        %subst(
038500030822     C                             %subst(vlrppt:posDaDft+1:
038600030822     C                             posADft-posDaDft-1):1:3)
038700130612     C                             = 'TBL'
038800130612     C                   EVAL      p_tasTBL=%trim(%subst(
038900030822     C                             %subst(vlrppt:posDaDft+1:
039000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039100030822     C                   ENDIF
039200030822     C* TSP
039300030822     C                   IF        %subst(
039400030822     C                             %subst(vlrppt:posDaDft+1:
039500030822     C                             posADft-posDaDft-1):1:3)
039600030822     C                             = 'TSP'
039700130612     C                   EVAL      p_tasTSP=%trim(%subst(
039800030822     C                             %subst(vlrppt:posDaDft+1:
039900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040000030822     C                   ENDIF
040100030822     C* CTM
040200030822     C                   IF        %subst(
040300030822     C                             %subst(vlrppt:posDaDft+1:
040400030822     C                             posADft-posDaDft-1):1:3)
040500030822     C                             = 'CTM'
040600130612     C                   EVAL      p_tasCTM=%trim(%subst(
040700030822     C                             %subst(vlrppt:posDaDft+1:
040800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040900030822     C                   ENDIF
041000030822     C* FFD
041100030822     C                   IF        %subst(
041200030822     C                             %subst(vlrppt:posDaDft+1:
041300030822     C                             posADft-posDaDft-1):1:3)
041400030822     C                             = 'FFD'
041500130612     C                   EVAL      p_tasFFD=%trim(%subst(
041600030822     C                             %subst(vlrppt:posDaDft+1:
041700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041800030822     C                   ENDIF
041900130612     C* inKSU
042000130612     C                   IF        %subst(
042100130612     C                             %subst(vlrppt:posDaDft+1:
042200130612     C                             posADft-posDaDft-1):1:3)
042300130612     C                             = 'KSU'
042400130612     C                   EVAL      inKSU=%trim(%subst(
042500130612     C                             %subst(vlrppt:posDaDft+1:
042600130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042700130612     C                   ENDIF
042800130612     C* inTIP
042900130612     C                   IF        %subst(
043000130612     C                             %subst(vlrppt:posDaDft+1:
043100130612     C                             posADft-posDaDft-1):1:3)
043200130612     C                             = 'TIP'
043300130612     C                   EVAL      inTIP=%trim(%subst(
043400130612     C                             %subst(vlrppt:posDaDft+1:
043500130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043600130612     C                   ENDIF
043700130612     C* inISV
043800130612     C                   IF        %subst(
043900130612     C                             %subst(vlrppt:posDaDft+1:
044000130612     C                             posADft-posDaDft-1):1:3)
044100130612     C                             = 'ISV'
044200130612     C                   EVAL      inISV=%trim(%subst(
044300130612     C                             %subst(vlrppt:posDaDft+1:
044400130612     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044500130612     C                   ENDIF
044600030822     C* ...
044700030822     C                   ENDIF
044800030822     C                   ENDDO
044900020204     C*
045000010330     C                   ENDSR
045100010607     C*----------------------------------------------------*
045200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
045300010607     C*----------------------------------------------------*
045400130612     C     IMPFILE       BEGSR
045500010607     C*
045600130612     C                   SETOFF                                       31
045700010607     C*
045800030715     C**********
045900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
046000030715     C**********
046100030715     C                   IF        wGiro = *zeros
046200040113     C*
046300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
046400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
046500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
046600080429     C                             %trim(intcol(5))<>*blanks
046700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
046800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
046900080429     C                                      %trim(intcol(5))
047000070823     C     ';':CharCSV   XLATE     vindta        vindta
047100040510     C                   ENDIF
047200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
047300031201     C     minu:maiu     XLATE     vindta        vindta
047400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047500030715     C                   DOW       posDa <= %len(%trim(vindta))
047600030715     C*
047700030715     C* Gestisco il 1° campo
047800030715     C                   IF        i = 1
047900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
048000030715     C                   EVAL      posDa = 1
048100030715     C* Eseguo lo scan x trovare la fine del primo campo
048200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048300030715     C* A questo punto "estraggo" il campo corrente
048400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048500030715     C                                                    (posA-posDa))
048600030715     C* X i campi successivi al 1°
048700030715     C                   ELSE
048800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
048900030715     C                   EVAL      posDa = posA + 1
049000030715     C* Eseguo lo scan x trovare la fine del campo corrente
049100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
049200030715     C* Gestisco l'ultimo campo
049300030715     C                   IF        posA = *zeros
049400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
049500030715     C* A questo punto "estraggo" il campo corrente
049600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
049700030715     C                                                    (posA-posDa))
049800030715     C                   LEAVE
049900030715     C                   ELSE
050000030715     C* A questo punto "estraggo" il campo corrente
050100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
050200030715     C                                                    (posA-posDa))
050300030715     C                   ENDIF
050400030715     C                   ENDIF
050500030715     C* Incremento il contatore d campo
050600030715     C                   EVAL      i = i +1
050700030715     C                   ENDDO
050800030715     C*
050900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
051000030715     C* delimitazione testo
051100030715     C                   EVAL      i = 1
051200030715     C                   DOW       i < %elem(SkSplitFLD)
051300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
051400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
051500030715     C                   EVAL      i = i + 1
051600030715     C                   ENDDO
051700030715     C                   EVAL      wGiro = 1
051800030715     C                   ELSE
051900030715     C                   EVAL      wGiro = 2
052000031204     C**********
052100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
052200031204     C* d separatore campo
052300031204     C**********
052400031204     C                   EVAL      vindta = %trim(vindta)
052500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
052600031204     C                   EVAL      vindta = %subst(vindta:2)
052700031204     C                   ENDDO
052800031204     C*
052900031204     C                   Z-ADD     *zeros        lunghInput        4 0
053000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
053100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
053200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
053300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
053400031204     C                   ENDDO
053500030715     C**********
053600030715     C* Effettuo lo split del campo dati d input
053700030715     C**********
053800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
053900030715     C                   DOW       posDa <= %len(%trim(vindta))
054000030715     C*
054100030715     C* Gestisco il 1° campo
054200030715     C                   IF        i = 1
054300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
054400030715     C                   EVAL      posDa = 1
054500030715     C* Eseguo lo scan x trovare la fine del primo campo
054600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
054700030715     C* A questo punto "estraggo" il campo corrente
054800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
054900030715     C                                                    (posA-posDa))
055000030715     C* X i campi successivi al 1°
055100030715     C                   ELSE
055200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
055300030715     C                   EVAL      posDa = posA + 1
055400031204     C* Verifico che nn vi sia il campo nn valorizzato
055500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
055600031204     C* Se campo nn valorizzato skippo al prossimo
055700031204     C                   EVAL      SkSplitCSV(i) = *blanks
055800031204     C                   EVAL      posA  = posA + 1
055900031204     C                   ELSE
056000030715     C* Eseguo lo scan x trovare la fine del campo corrente
056100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
056200030715     C* Gestisco l'ultimo campo
056300030715     C                   IF        posA = *zeros
056400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
056500030715     C* A questo punto "estraggo" il campo corrente
056600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
056700030715     C                                                    (posA-posDa))
056800030715     C                   LEAVE
056900030715     C                   ELSE
057000030715     C* A questo punto "estraggo" il campo corrente
057100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
057200030715     C                                                    (posA-posDa))
057300030715     C                   ENDIF
057400031204     C                   ENDIF
057500030715     C                   ENDIF
057600030715     C* Incremento il contatore d campo
057700030715     C                   EVAL      i = i +1
057800030715     C                   ENDDO
057900030715     C*
058000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
058100030715     C* delimitazione testo
058200030715     C                   EVAL      i = 1
058300030715     C                   DOW       i < %elem(SkSplitCSV)
058400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
058500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
058600030715     C                   EVAL      i = i + 1
058700030715     C                   ENDDO
058800100326     C*
058900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
059000030715     C                   EVAL      i = 1
059100030715     C                   DOW       i < %elem(SkSplitFLD)
059200030715     C*
059300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
059400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
059500030715     C                             SkSplitCSV(i) <> *blanks
059600070823     C*
059700130612 xxx C***  ===>  Gestione campi "particolari" dentro ciclo
059800110421     C*
059900130612     C*** RMN
060000130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMN'
060100130612     C                   EVAL      PiStr=SkSplitCSV(i)
060200130612     C                   EXSR      CHKNUM
060300130612     C                   IF        PiInt=*on
060400130612     C                   Z-ADD     PiVal         w_tasRMN
060500130612     C                   ELSE
060600130612     C                   SETON                                        31
060700130612     C                   EVAL      w_tasRMN = *zeros
060800130612     C                   EVAL      vinmsg = %trimr(vinmsg)
060900130612     C                             + ' ' + 'INRMN'
061000130612     C                   ENDIF
061100130612     C                   ENDIF
061200130612     C*** AAS
061300130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INAAS'
061400130612     C                   EVAL      PiStr=SkSplitCSV(i)
061500130612     C                   EXSR      CHKNUM
061600130612     C                   IF        PiInt=*on
061700130612     C                   Z-ADD     PiVal         wDATA             8 0
061800130612     C                   MOVEL     wDATA         w_tasAAS
061900130612     C                   ELSE
062000130612     C                   SETON                                        31
062100130612     C                   EVAL      w_tasAAS = *zeros
062200130612     C                   EVAL      vinmsg = %trimr(vinmsg)
062300130612     C                             + ' ' + 'INAAS'
062400130612     C                   ENDIF
062500130612     C                   ENDIF
062600130612     C*** POR
062700130612     C                   IF        %trim(SkSplitFLD(i)) = 'W_INPOR'
062800130612     C*
062900130612     C* Effettuo la chiamata al *pgm d "bonifica" x il campo 'W_INPOR'
063000130612     C                   EVAL      TxtInOut   = SkSplitCSV(i)
063100130612     C                   EVAL      ElencoChar = '1234567890,.'
063200130612     C                   EVAL      EsitoBon   = *blanks
063300130612     C*
063400130612     C                   CALL      'XCHKCHAR'
063500130612     C                   PARM                    TxtInOut
063600130612     C                   PARM                    ElencoChar
063700130612     C                   PARM                    TipoElenco
063800130612     C                   PARM                    CharSost
063900130612     C                   PARM                    UpperCase
064000130612     C                   PARM                    ChkNull
064100130612     C                   PARM                    CharNull
064200130612     C                   PARM                    EsitoBon
064300130612     C*
064400130612     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
064500130612     C                   IF        EsitoBon = '1'
064600130612     C                   EVAL      SkSplitCSV(i) = TxtInOut
064700130612     C                   ENDIF
064800130612     C*
064900130612     C                   EVAL      PiStr=SkSplitCSV(i)
065000130612     C                   EXSR      CHKNUM
065100130612     C                   IF        PiNum=*on
065200130612     C                   Z-ADD     PiVal         w_tasPOR
065300130612     C                   ELSE
065400130612     C                   SETON                                        31
065500130612     C                   EVAL      w_tasPOR = *zeros
065600130612     C                   EVAL      vinmsg = %trimr(vinmsg)
065700130612     C                             + ' ' + 'INPOR'
065800130612     C                   ENDIF
065900130612     C                   ENDIF
066000130612     C*** RSD
066100130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRSD'
066200130612     C                   EVAL      w_tasRSD = SkSplitCSV(i)
066300110421     C                   ENDIF
066400130612     C*** LOD
066500130612     C                   IF        %trim(SkSplitFLD(i)) = 'INLOD'
066600130612     C                   EVAL      w_tasLOD = SkSplitCSV(i)
066700130612     C                   ENDIF
066800130612     C*** RMA
066900130612     C                   IF        %trim(SkSplitFLD(i)) = 'INRMA'
067000130612     C                   EVAL      w_tasRMA = SkSplitCSV(i)
067100130612     C                   ENDIF
067200070925     C*
067300071029     C***  <===  -----------------------------------------
067400040510     C*
067500030715     C                   ENDIF
067600030715     C                   EVAL      i = i + 1
067700030715     C                   ENDDO
067800070925     C*
067900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
068000130612     C*
068100130612     C* Aggancio la bolla in sede
068200130612     C                   movel     'N'           wOK               1
068300130612     C     KEYtas32C     setll     titas32c
068400130612     C                   if        %equal(titas32c)
068500130612     C     KEYtas32C     reade     titas32c
068600130612     C                   dow       not %eof(titas32c)
068700130612     C                   movel     'S'           wOK
068800130612     C*
068900130612     C* Se richiesti applico i filtri
069000130612     C*
069100130612     C                   if        w_tasAAS > *zeros
069200130612     C                   if        w_tasAAS = tasAAS
069300130612     C                   else
069400130612     C                   movel     'N'           wOK
069500130612     C                   endif
069600130612     C                   endif
069700130612     C*
069800130612     C                   if        p_tasCCM > *zeros
069900130612     C                   if        p_tasCCM = tasCCM
070000130612     C                   else
070100130612     C                   movel     'N'           wOK
070200130612     C                   endif
070300130612     C                   endif
070400130612     C*
070500130612     C                   if        p_tasLNP > *zeros
070600130612     C                   if        p_tasLNP = tasLNP
070700130612     C                   else
070800130612     C                   movel     'N'           wOK
070900130612     C                   endif
071000130612     C                   endif
071100130612     C*
071200130612     C                   if        p_tasNRS > *zeros
071300130612     C                   if        p_tasNRS = tasNRS
071400130612     C                   else
071500130612     C                   movel     'N'           wOK
071600130612     C                   endif
071700130612     C                   endif
071800130612     C*
071900130612     C                   if        p_tasCTR > *zeros
072000130612     C                   if        p_tasCTR = tasCTR
072100130612     C                   else
072200130612     C                   movel     'N'           wOK
072300130612     C                   endif
072400130612     C                   endif
072500130612     C*
072600130612     C                   if        p_tasTBL <> *blanks
072700130612     C                   if        p_tasTBL  = tasTBL
072800130612     C                   else
072900130612     C                   movel     'N'           wOK
073000130612     C                   endif
073100130612     C                   endif
073200130612     C*
073300130612     C                   if        p_tasTSP <> *blanks
073400130612     C                   if        p_tasTSP  = tasTSP
073500130612     C                   else
073600130612     C                   movel     'N'           wOK
073700130612     C                   endif
073800130612     C                   endif
073900130612     C*
074000130612     C                   if        p_tasCTM <> *blanks
074100130612     C                   if        p_tasCTM  = tasCTM
074200130612     C                   else
074300130612     C                   movel     'N'           wOK
074400130612     C                   endif
074500130612     C                   endif
074600130612     C*
074700130612     C                   if        p_tasFFD <> *blanks
074800130612     C                   if        p_tasFFD  = tasFFD
074900130612     C                   else
075000130612     C                   movel     'N'           wOK
075100130612     C                   endif
075200130612     C                   endif
075300130612     C*
075400130612     C* Aggancio il file estensione riferimenti bolle - tipo record 'A'
075500130612     C                   clear                   ta4NOT
075600130612     C                   eval      ta4TRC = 'A'
075700130612     C     KEYta430      chain     tita430c
075800130612     C                   if        %found(tita430c)
075900130612     C*
076000130612     C                   if        w_tasRMA <> *blanks
076100130612     C                   if        w_tasRMA  = ta4NOT
076200130612     C                   else
076300130612     C                   movel     'N'           wOK
076400130612     C                   endif
076500130612     C                   endif
076600130612     C*
076700130612     C                   endif
076800130612     C*
076900130612     C     KEYtas32C     reade     titas32c
077000130612     C                   enddo
077100130612     C                   endif
077200070925     C*
077300071029     C***  <===  -----------------------------------------
077400070925     C*
077500040510     C*
077600130612     C* Considerazioni finali sul confronto fattura (solo su PORTO)
077700130612     C                   if        *in31
077800130612     C                   eval      wStatus = 'ERR_INPUT'
077900130612     C                   else
078000130612     C                   if        wOK <> 'S'
078100130612     C                   eval      wStatus = 'MANCA BRT'
078200130612     C                   else
078300130612     C                   z-add     tasPOR        wPorto
078400130612     C                   if        wPorto  = w_tasPOR
078500130612     C                   eval      wStatus = 'CONFORME'
078600130612     C                   else
078700130612     C                   eval      wStatus = 'DIFFERENZA'
078800130612     C                   endif
078900130612     C                   endif
079000130612     C                   endif
079100130612     C*
079200130612     C                   ENDIF
079300020204     C*
079400010607     C                   ENDSR
079500010607     C*----------------------------------------------------*
079600010330
079700100318
079800100318
079900100318     C*----------------------------------------------------*
080000100318     C*  CONTROLLO NUMERICITA' CAMPI
080100100318     C*----------------------------------------------------*
080200100318     C     CHKNUM        BEGSR
080300100318     C*
080400100318     C                   IF        PiDecChr = *blanks
080500100318     C                   EVAL      PiDecChr = CharNUM
080600100318     C                   ENDIF
080700100318     C*
080800100318     C                   callp     UBISNUM_Check(PiStr
080900100318     C                                          :PiDecChr
081000100318     C                                          :PiVal
081100100318     C                                          :PiNum
081200100318     C                                          :PiInt)
081300100318     C*
081400100318     C                   ENDSR
081500010601     C***
081600010601
081700010601
081800010601
081900010601      /TITLE Invio dei dati al punto operativo.
082000010601     C     opeini        BEGSR
082100010601     C*
082200010601     C* Inizializzo flag e contatori operativi
082300010601     C                   movel     '0'           flgGiro           1
082400010601     C                   movel     '0'           flgMulti          1
082500010601     C                   movel     '1'           flgStato          1
082600010615     C                   movel     '0'           flgOk             1
082700010601     C                   z-add     *zeros        cntNonEl         10 0
082800010601     C                   z-add     *zeros        invfil            3 0
082900130612     C*
083000130612     C* Scrivo la riga di intestazione colonne del file di output
083100130612     C                   eval      wDati = 'SPED BRT;'+
083200130612     C                                     'RIF_N MITT;'+
083300130612     C                                     'RIF_A MITT;'+
083400130612     C                                     'DESTINATARIO;'+
083500130612     C                                     'LOCALITA;'+
083600130612     C                                     'FATT CLIENTE;'+
083700130612     C                                     'FATT BRT;'+
083800130612     C                                     'STATO;'
083900130612     C                   exsr      wriVGD
084000010601     C*
084100010601     C                   ENDSR
084200010601     C***
084300100318
084400100318
084500100318
084600100318     C     normCSV       BEGSR
084700100318     C*
084800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
084900100318     C* separatori campi
085000100318     C                   if        CharTXT <> *blanks
085100100318     C                   z-add     *zeros        wLenVINDTA        4 0
085200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
085300100318     C                   setoff                                       80
085400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
085500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
085600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
085700100318     C                   if        not *in80
085800100318     C                   seton                                        80
085900100318     C                   else
086000100318     C                   setoff                                       80
086100100318     C                   endif
086200100318     C                   endif
086300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
086400100318     C                             and *in80 = *on
086500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
086600100318     C                   endif
086700100318     C                   enddo
086800100318     C                   endif
086900100318     C*
087000100318     C                   ENDSR
087100100318     C***
087200130612
087300130612
087400130612
087500130612     C     strTRUL47     BEGSR
087600130612     C*
087700130612     C                   movel     'N'           wProcedi          1
087800130612     C*
087900130612     C* Avvio il blocco elaborazione TIVGD x tipo file in questione
088000130612     C                   clear                   trul47ds
088100130612     C                   eval      d47opz  = 'I'
088200130612     C                   eval      d47tip  = inTIP
088300130612     C                   eval      d47lck  = 'N'
088400130612     C                   eval      d47chkj = 'S'
088500130612     C                   eval      d47pgm  = 'TITV2Q4R'
088600130612     C                   call      'TRUL47R'
088700130612     C                   parm                    trul47ds
088800130612     C*
088900130612     C* Se elaborazione consentita => proseguo
089000130612     C                   if        d47sts <> 'A'
089100130612     C                   movel     'S'           wProcedi
089200130612     C                   endif
089300130612     C*
089400130612     C                   ENDSR
089500130612
089600130612
089700130612
089800130612     C     endTRUL47     BEGSR
089900130612     C*
090000130612     C                   if        wProcedi = 'S'
090100130612     C*
090200130612     C* Elimino il blocco elaborazione TIVGD x tipo file in questione
090300130612     C                   clear                   trul47ds
090400130612     C                   eval      d47opz  = 'F'
090500130612     C                   eval      d47tip  = inTIP
090600130612     C                   call      'TRUL47R'
090700130612     C                   parm                    trul47ds
090800130612     C*
090900130612     C                   endif
091000130612     C*
091100130612     C                   ENDSR
091200130612
091300130612
091400130612
091500130612
091600130612      /TITLE Valorizzazione Progressivo Applicazione
091700130612     C     calprog       begsr
091800130612     C*
091900130612     C                   movel     *blanks       dwlisv            2
092000130612     C                   movel     *all'0'       dwlprg           10
092100130612     C                   z-add     *zeros        wrkprg            8 0
092200130612     C*
092300130612     C                   eval      dwlisv = inISV
092400130612     C*
092500130612     C                   open      tis7prgf
092600130612     C*
092700130612     C                   read(e)   tis7prgf
092800130612     C                   if        not %error
092900130612     C                   eval      dwlprg = f_tis7prgf
093000130612     C*
093100130612     C                   move(p)   dwlprg        wrkprg
093200130612     C                   add       1             wrkprg
093300130612     C                   move(p)   wrkprg        dwlprg
093400130612     C                   movel     dwlisv        dwlprg
093500130612     C*
093600130612     C                   eval      f_tis7prgf = dwlprg
093700130612     C                   update    tis7prg0
093800130612     C                   endif
093900130612     C*
094000130612     C                   close     tis7prgf
094100130612     C*
094200130612     C                   endsr
094300010330
094400100326
094500010330
094600000613     C     *inzsr        BEGSR
094700990910     C*
094800990910     C     *entry        plist
094900130612     C                   parm                    tivlrds
095000990921     C                   parm      wrkesito      esito
095100000724     C                   parm                    prmlit
095200000710     C                   parm                    prmfir
095300010330     C*
095400010330     C* CALCOLA LA DATA CORRENTE
095500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
095600100318     C                   eval      datcor = %dec(%date() : *ISO)
095700130612     C*
095800130612     C* Definizione chiavi
095900130612     C*
096000130612     C* Chiave su TITAS32C - Completa
096100130612     C     KEYtas32C     KLIST
096200130612     C                   KFLD                    w_tasRMN
096300130612     C                   KFLD                    p_tasCCM
096400130612     C*
096500130612     C* Chiave su TITA430C - Completa
096600130612     C     KEYta430      KLIST
096700130612     C                   KFLD                    tasAAS
096800130612     C                   KFLD                    tasLNP
096900130612     C                   KFLD                    tasNRS
097000130612     C                   KFLD                    tasNSP
097100130612     C                   KFLD                    ta4TRC
097200000613     C*
097300000613     C                   ENDSR
097400000613     C***
097500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
097600130612INRMN;W_INAAS;INRSD;INLOD;W_INPOR;INRMA
097700110421
097800110315
097900110315
098000100318
