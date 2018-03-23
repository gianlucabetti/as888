000100000000     h*--------------------------------------------------------------------------------------------*
000200130822     h* Utility creaz. file AZCOS00F - orari cappario
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* Data base
000700000000     f*--------------------------------------------------------------------------------------------*
000800130822     ftisio3d   CF   E             WORKSTN
000900130822     fazorg01l  IF   E           K DISK
001000130822     fazcpr01l  IF   E           K DISK
001100000000     fazcpl01l  IF   E           K DISK
001200130822     fazcos01l  UF A E           K DISK
001300130823     fqsysprt   O    F  132        PRINTER OFLIND(*INOF) FORMOFL(64)
001400130823     f                                                   FORMLEN(66)
001500130823     f                                                   usropn
001600000000     d*--------------------------------------------------------------------------------------------*
001700000000     d* Data structure
001800000000     d*--------------------------------------------------------------------------------------------*
001900000726     d* schiere a tempo di compilazione
002000000000     d*----------------
002100130822     d msg             S             78    DIM(15) CTDATA PERRCD(1)             *messaggi video
002200000000     d sta             S             66    DIM(10) CTDATA PERRCD(1)             *righe di stampa
002300130823     d msp             S             45    DIM(10) CTDATA PERRCD(1)             *messaggi stampa
002400130823     d num             S              1    DIM(10) CTDATA PERRCD(10)
002500000000     d*----------------
002600000000     d* Variabili riferite al data base
002700000000     d*----------------
002800130822     d knar            S                   LIKE(cosnar)
002900130822     d kcap            S                   LIKE(coscap)
003000130822     d kloc            S                   LIKE(cosloc)
003100130830     d kkcap           S                   LIKE(coscap)
003200130830     d kkloc           S                   LIKE(cosloc)
003300130822     d klna            S                   LIKE(coslna)
003400000000     d*----------------
003500000000     d* Variabili di controllo
003600000000     d*----------------
003700000000     d $fine           S              1    INZ('N')                             *Gestione
003800000000     d $err            S              1    INZ('0')                             *errore
003900000000     d $tesvd          S              1    INZ('1')                             *riemettere testata
004000000000     d $tipvid         S              1    INZ('1')                             *gestione video
004100000000     d*----------------
004200000000     d* Variabili a programma
004300000000     d*----------------
004400000726     d s               S              5  0                                      *indice
004500000726     d i               S              5  0                                      *indice
004600000726     d n14             S             14  0                                      *numerico 14
004700000000     d n8              S              8  0                                      *numerico 8
004800130823     d Indx            S              4  0
004900130823     d YY              S              4  0
005000000000     d datcor          S              8  0                                      *data corrente a/m/g
005100130822     d Welab           S              1
005200130822     d ktser           S              1
005300130823     d Wfile           S              1
005400130823     d kdlna           S              5
005500130822     d wrkgetlista     s           4096    varying
005600130823     d staesito        S             45
005700000000     d p96dri          S                   LIKE(cprdde)                         *lancio TISI96R
005800000000     d p96ver          S                   LIKE(cprver)
005900130822
006000130822     d Datasys         s               d   datfmt(*iso) inz(*sys)
006100000000     d*----------------
006200000000     d* Ds
006300000000     d*----------------
006400000000     d dssi00        E DS                  EXTNAME(tisi00ds)                    reperimento versioni
006500130822     d azcpcds       E DS                  EXTNAME(azcpc00f)
006600130823     d azcplds       E DS                  EXTNAME(azcpl00f)
006700130823     d azcosDS       E DS                  EXTNAME(azcos00f)
006800130823     d azcosSAV      E DS                  EXTNAME(azcos00f) prefix(s_)
006900000000     d kpjba         E DS                                                       architettura
007000130822     d og143         E DS
007100130822     d
007200130822     d                 DS
007300130822     d  wHH                    1      2  0
007400130822     d  wMM                    3      4  0
007500130822     d WORA                    1      4  0
007600130822     d
007700000000     c*--------------------------------------------------------------------------------------------*
007800000000     c* Main lines
007900000000     c*--------------------------------------------------------------------------------------------*
008000000000     C*
008100000000     C* impostazioni testata
008200000000     C                   EXSR      impvd0
008300000000     C*
008400000000     C* impostazioni prima videata
008500000000     C                   EXSR      impvd1
008600000000     C*
008700000000     C* ciclo fino a richiesta di fine elaborazione
008800000000do  1C                   DOU       $fine='S'
008900000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
009000000000     C                   ENDCS
009100000000e   1C                   ENDDO
009200000000     c*
009300000000     c* operazioni finali
009400000000     c                   EXSR      finsr
009500000000     C*
009600000000     C                   SETON                                        lr
009700000000     c*--------------------------------------------------------------------------------------------*
009800000000     c* impvd1 - impostazioni prima videata
009900000000     c*--------------------------------------------------------------------------------------------*
010000000000     C     impvd1        BEGSR
010100000000     c*
010200130829     c* Reperisce l'ultima versione
010300130822     c                   movel     datasys       p96dri                         *ultima versione
010400000000     c                   Z-ADD     *zeros        p96ver
010500000000     c                   CALL      'TISI96R'
010600000000     c                   PARM                    p96dri
010700000000     c                   PARM                    p96ver
010800000000     c                   Z-ADD     p96ver        v1cver                         *versione
010900000000     C                   MOVEL     'S'           v1csim                         *simulazione
011000130829     c
011100130829     c* tolto campo dal video imposto sempre T- aggiornamento sempre
011200130829     C                   MOVEL     'T'           v1caggio          1
011300130829     c
011400130822     C                   clear                   v1ccapd
011500130822     C                   clear                   v1ccapa
011600130822     C                   clear                   v1clna
011700130829     c* Campo eliminato: aggiorno solo dai CAP
011800130829     C                   MOVEL     'C'           v1ctfile          1
011900130829     c* Campo eliminato  lascio sempre ' '
012000130829     C                   MOVEL     ' '           v1ctlna           1
012100130829     c* Carico dai dati aziendali record 999
012200130829     c                   clear                   kloc
012300130829     c                   clear                   kcap
012400130829     c                   clear                   knar
012500130829     c                   eval      klna=999
012600130829     c                   movel     'R'           ktser
012700130829     C                   movel     'RITIRI'      v1rtser
012800130829     c     kcos          chain     azcos01l
012900130829     c                   if        %found(azcos01l)
013000130829     c                   movel     cosostis      V1ROSTIS
013100130829     c                   movel     cosostfs      V1ROSTfS
013200130829     c                   movel     cosomiis      V1ROmiis
013300130829     c                   movel     cosomxfs      V1ROmxfs
013400130829     c                   movel     costrzs       V1Rtrzs
013500130829     c                   else
013600130829     c*
013700130822     c                   movel     1200          V1ROSTIS
013800130822     c                   movel     1700          V1ROSTfS
013900130822     c                   movel     1030          V1ROmiis
014000130822     c                   movel     1800          V1ROmxfs
014100130829     c                   endif
014200130829     C
014300130822     C                   movel     'CONSEGNE'    v1ctser
014400130829     c                   movel     'C'           ktser
014500130829     c     kcos          chain     azcos01l
014600130829     c                   if        %found(azcos01l)
014700130829     c                   movel     cosostis      V1COSTIS
014800130829     c                   movel     cosostfs      V1COSTfS
014900130829     c                   movel     cosomiis      V1COmiis
015000130829     c                   movel     cosomxfs      V1COmxfs
015100130829     c                   movel     costrzs       V1Ctrzs
015200130829     c                   else
015300130822     c                   movel     0830          V1cOSTIS
015400130822     c                   movel     1800          V1cOSTfS
015500130822     c                   movel     0730          V1cOmiis
015600130822     c                   movel     1830          V1cOmxfs
015700130829     c                   endif
015800000000     C*
015900000000     C* imposta formato video e indicatori
016000000000     C                   EXSR      setvd1
016100000000     C                   MOVEL     '1'           $tipvid                        *gestione video
016200000000     C*
016300000000     C                   ENDSR
016400000000     c*--------------------------------------------------------------------------------------------*
016500000000     c* setvd1 - impostazioni indicatori prima videata
016600000000     c*--------------------------------------------------------------------------------------------*
016700000000     C     setvd1        BEGSR
016800000000     C*
016900000000     C                   SETOFF                                       202122     *errori
017000000726     C                   SETOFF                                       232425
017100130822     C                   SETOFF                                       404142
017200130822     C                   SETOFF                                       434445
017300130822     C                   SETOFF                                       464748
017400130822     C                   SETOFF                                       49
017500130822     C                   SETOFF                                       9028       *generico di errore
017600130822     C                   MOVEL     *blanks       v1cmsg                          *messaggio
017700000000     C*
017800000000     C                   ENDSR
017900000000     c*--------------------------------------------------------------------------------------------*
018000000000     C* gesvd1 - gestione prima videata
018100000000     c*--------------------------------------------------------------------------------------------*
018200000000     C     gesvd1        BEGSR
018300000000     C*
018400000000     C* emissione videata
018500000000if  1C                   IF        $tesvd='1'
018600130822     C                   WRITE     sio3d00                                      *testata
018700000000     C                   MOVEL     '0'           $tesvd                         *no testata
018800000000e   1C                   ENDIF
018900130822     C                   EXFMT     sio3d01                                      *video
019000000000     C                   EXSR      setvd1
019100000000     C* F03-Fine
019200000000if  1C                   IF        *inkc
019300000000     C                   EXSR      f03ges
019400000000     C                   GOTO      finvd1
019500000000e   1C                   ENDIF
019600000000     C* F10-Ricerca
019700000000if  1C                   IF        *inkj
019800000000     C                   EXSR      f10ges
019900000000     C                   GOTO      finvd1
020000000000e   1C                   ENDIF
020100000000     C* F12-Ritorno
020200000000if  1C                   IF        *inkl
020300000000     C                   EXSR      f12ges
020400000000     C                   GOTO      finvd1
020500000000e   1C                   ENDIF
020600000000     C* Controlla video
020700000000     C                   EXSR      ctrvd1
020800000000if  1C                   IF        *in90                                        *errori
020900000000     C                   GOTO      finvd1
021000000000e   1C                   ENDIF
021100000000     C* F06-Conferma
021200000000if  1C                   IF        *inkf
021300000000     C                   EXSR      f06ges
021400000000     C                   GOTO      finvd1
021500000000e   1C                   ENDIF
021600000000     C*
021700000000     C     finvd1        ENDSR
021800000000     c*--------------------------------------------------------------------------------------------*
021900000000     C* ctrvd1 - controlla prima videata
022000000000     c*--------------------------------------------------------------------------------------------*
022100000000     C     ctrvd1        BEGSR
022200000000     C*
022300000000     C* reimposta le variabili di lavoro
022400000000     C                   MOVEL     '0'           $err
022500000000     C*---
022600000000     C* simulazione
022700000000     C*---
022800000000if  1c                   IF        v1csim = 'S' OR
022900000000     c                             v1csim = 'N'
023000000000x   1c                   ELSE
023100130822     C                   SETON                                        209028
023200130822     C                   MOVEL     msg(1)        v1cmsg
023300000000     C                   MOVEL     '1'           $err
023400000000     C                   GOTO      finct1
023500000000e   1C                   ENDIF
023600000000     C*---
023700000000     C* versione
023800000000     C*---
023900000000     C                   MOVEL     *blanks       v1dver
024000130822     c     v1cver        CHAIN     azcpr01l
024100000000     c                   IF        %FOUND
024200000000     c                   MOVEL     cprdes        v1dver
024300000000     c                   ELSE
024400130822     C                   SETON                                        219028
024500000000     C                   MOVEL     '1'           $err
024600130822     C                   MOVEL     msg(2)        v1cmsg
024700000000     C                   GOTO      finct1
024800000000     c                   ENDIF
024900130822     c* CAP dal/al
025000130822     c                   if        (v1ccapd=*blanks and v1ccapa<>*blanks) or
025100130822     c                             (v1ccapd<>*blanks and v1ccapa= *blanks)
025200130822     C                   SETON                                        229028
025300130822     C                   MOVEL     '1'           $err
025400130822     C                   MOVEL     msg(3)        v1cmsg
025500130822     C                   GOTO      finct1
025600130822     c                   endif
025700130822     c
025800130822     c                   if        v1ccapd<>*blanks and
025900130822     c                             v1ccapd> v1ccapa
026000130822     C                   SETON                                        229028
026100130822     C                   MOVEL     '1'           $err
026200130822     C                   MOVEL     msg(4)        v1cmsg
026300130822     C                   GOTO      finct1
026400130822     c                   endif
026500130823     c
026600130823     c* Il CAP deve contenere 5 cifre essendo per ora solo CAP italiani
026700130823     c                   if        v1ccapd<>*blanks
026800130823     c                   z-add     1             yy
026900130823     c                   do        5             yy
027000130823     c                   eval      Indx=%lookup(%subst(v1ccapd:yy:1):num)
027100130823     c                   if        indx=0
027200130823     C                   SETON                                        229028
027300130823     C                   MOVEL     '1'           $err
027400130823     C                   MOVEL     msg(14)       v1cmsg
027500130823     C                   GOTO      finct1
027600130823     c                   endif
027700130823     c                   enddo
027800130823
027900130823     c                   z-add     6             yy
028000130823     c     6             do        9             yy
028100130823     c                   if        %subst(v1ccapd:yy:1)<>' '
028200130823     C                   SETON                                        229028
028300130823     C                   MOVEL     '1'           $err
028400130823     C                   MOVEL     msg(14)       v1cmsg
028500130823     C                   GOTO      finct1
028600130823     c                   endif
028700130823     c                   enddo
028800130823     c                   endif
028900130823     c
029000130823     c* Il CAP deve contenere 5 cifre essendo per ora solo CAP italiani
029100130823     c                   if        v1ccapa<>*blanks
029200130823     c                   z-add     1             yy
029300130823     c                   do        5             yy
029400130823     c                   eval      Indx=%lookup(%subst(v1ccapa:yy:1):num)
029500130823     c                   if        indx=0
029600130823     C                   SETON                                        259028
029700130823     C                   MOVEL     '1'           $err
029800130823     C                   MOVEL     msg(14)       v1cmsg
029900130823     C                   GOTO      finct1
030000130823     c                   endif
030100130823     c                   enddo
030200130823
030300130823     c                   z-add     6             yy
030400130823     c     6             do        9             yy
030500130823     c                   if        %subst(v1ccapa:yy:1)<>' '
030600130823     C                   SETON                                        259028
030700130823     C                   MOVEL     '1'           $err
030800130823     C                   MOVEL     msg(14)       v1cmsg
030900130823     C                   GOTO      finct1
031000130823     c                   endif
031100130823     c                   enddo
031200130823     c                   endif
031300130823
031400130823     c* CAP o linea di arrivo obbligatori
031500130823     c                   if        v1ccapd=*blanks and (v1clna=*zeros
031600130823     c                             or v1clna=*blanks)
031700130823     C                   SETON                                        229028
031800130823     C                   MOVEL     '1'           $err
031900130823     C                   MOVEL     msg(15)       v1cmsg
032000130823     C                   GOTO      finct1
032100130823     c                   endif
032200130823     c
032300130822     c* linea di arrivo
032400130822     c                   clear                   comlna
032500130822     C     '?'           SCAN      v1clna                                 30
032600130822     C*
032700130822    2C     *IN30         IFEQ      *ON
032800130822     C                   MOVE      *BLANKS       §COD1
032900130822     C                   MOVE      *BLANKS       v1clna
033000130823     c                   movel     'A'           §des
033100130822     C                   CALL      'TNSD24R'
033200130822     C                   PARM                    §COD1             3
033300130822     C                   PARM                    §TIP              1
033400130822     C                   PARM                    §DES             25
033500130823     C                   PARM                    §pra              3
033600130822     C                   MOVEL     §COD1         v1clna
033700130822     C                   MOVEL     §DES          V1DLNA
033800130822     c                   MOVEL     '1'           $tesvd
033900130822     C                   MOVEL     '1'           $err
034000130822     C                   SETON                                        2390
034100130822     C                   GOTO      finct1
034200130822    2C                   ENDIF
034300130822     c
034400130822     c                   if        v1clna<>*blanks and v1clna<>*zeros
034500130822
034600130822     C                   TESTN                   v1clna               30
034700130822     C                   MOVE      v1clna        W001A             1
034800130822    2C     *IN30         IFEQ      *OFF
034900130822     C     W001A         ORLT      '0'
035000130822     C                   MOVEL     msg(5)        V1cmSG
035100130822     C                   SETON                                        902823
035200130822     C                   MOVEL     '1'           $err
035300130822     C                   leavesr
035400130822    2C                   ENDIF
035500130822    2C     v1clna        IFGT      *ZEROS
035600130822     C                   MOVEL     v1clna        COMLNA            3 0
035700130822     C     COMLNA        CHAIN     AZORG01L                           30
035800130822     C                   ELSE
035900130822     C                   SETON                                        30
036000130822    2C                   END
036100130822     C*
036200130822    2C  N30ORGFVA        IFNE      *BLANKS
036300130822    2C     ORGFAG        ORNE      'F'
036400130822     C     ORGFAG        ANDNE     'A'
036500130822     C                   SETON                                        30
036600130822    2C                   END
036700130822     c
036800130822    2C     *IN30         IFEQ      *ON
036900130822     C                   MOVEL     msg(5)        V1cMSG
037000130822     C                   SETON                                        902823
037100130822     C                   MOVEL     '1'           $err
037200130822     C                   leavesr
037300130822    2C                   ENDIF
037400130822
037500130822     c* non accetto linea estera
037600130822     c                   movel     orgde3        og143
037700130822     c                   if        §ogntw='EEX' or §ogntw='DPD' or
037800130822     c                             §ogntw='FED'
037900130822     C                   MOVEL     msg(6)        V1cMSG
038000130822     C                   SETON                                        902823
038100130822     C                   MOVEL     '1'           $err
038200130822     C                   leavesr
038300130822    2C                   ENDIF
038400130822
038500130822    2C                   ENDIF
038600130822     c* se la linea di arrivo è piena, non indicare il tipo linea, altrimenti è da indicare
038700130822     c                   if        comlna>0 and v1ctlna<>' '
038800130822     C                   MOVEL     msg(12)       V1cMSG
038900130822     C                   SETON                                        902824
039000130822     C                   MOVEL     '1'           $err
039100130822     C                   leavesr
039200130822    2C                   ENDIF
039300130822     c                   if        comlna=0 and v1ctlna= ' '
039400130822     C                   MOVEL     msg(13)       V1cMSG
039500130830     C                   SETON                                        902823
039600130822     C                   MOVEL     '1'           $err
039700130822     C                   leavesr
039800130822    2C                   ENDIF
039900130822
040000130822      /free
040100130822       // ORARI :  Ritiri
040200130822       if  v1rostis=0  ;
040300130822          *in90=*on    ;
040400130822          *in28=*on    ;
040500130822          *in40=*on    ;
040600130822          $err='1'     ;
040700130822          v1cmsg = Msg(07);
040800130822          leavesr;
040900130822 1     endif  ;
041000130822
041100130822       wora=v1rostis ;
041200130822       exsr contr_ora ;
041300130822       if v1cmsg<>*blanks ;
041400130822          *in90=*on    ;
041500130822          *in28=*on    ;
041600130822          *in40=*on    ;
041700130822          $err='1'     ;
041800130822          leavesr;
041900130822 1     endif  ;
042000130822
042100130822 1     if  v1rostfs=0     ;
042200130822          *in90=*on    ;
042300130822          *in28=*on    ;
042400130822          *in41=*on    ;
042500130822          $err='1'     ;
042600130822          v1cmsg = Msg(07);
042700130822          leavesr;
042800130822 1     endif  ;
042900130822
043000130822       wora=v1rostfs ;
043100130822       exsr contr_ora ;
043200130822       if v1cmsg<>*blanks ;
043300130822          *in90=*on    ;
043400130822          *in28=*on    ;
043500130822          *in41=*on    ;
043600130822          $err='1'     ;
043700130822          leavesr;
043800130822 1     endif  ;
043900130822
044000130822 1     if  v1rostfs<v1rostis  ;
044100130822          *in90=*on    ;
044200130822          *in28=*on    ;
044300130822          *in41=*on    ;
044400130822          $err='1'     ;
044500130822          v1cmsg = Msg(08);
044600130822          leavesr;
044700130822 1     endif  ;
044800130822
044900130822 1     if  v1romiis=0     ;
045000130822          *in90=*on    ;
045100130822          *in28=*on    ;
045200130822          *in42=*on    ;
045300130822          $err='1'     ;
045400130822          v1cmsg = Msg(07);
045500130822          leavesr;
045600130822 1     endif  ;
045700130822
045800130822       wora=v1romiis ;
045900130822       exsr contr_ora ;
046000130822       if v1cmsg<>*blanks ;
046100130822          *in90=*on    ;
046200130822          *in28=*on    ;
046300130822          *in42=*on    ;
046400130822          $err='1'     ;
046500130822          leavesr;
046600130822 1     endif  ;
046700130822 1     if  v1romxfs=0     ;
046800130822          *in90=*on    ;
046900130822          *in28=*on    ;
047000130822          *in43=*on    ;
047100130822          $err='1'     ;
047200130822          v1cmsg = Msg(07);
047300130822          leavesr;
047400130822 1     endif  ;
047500130822
047600130822       wora=v1romxfs ;
047700130822       exsr contr_ora ;
047800130822       if v1cmsg<>*blanks ;
047900130822          *in90=*on    ;
048000130822          *in28=*on    ;
048100130822          *in43=*on    ;
048200130822          $err='1'     ;
048300130822          leavesr;
048400130822 1     endif  ;
048500130822
048600130822 1     if  v1romxfs<v1romiis  ;
048700130822          *in90=*on    ;
048800130822          *in28=*on    ;
048900130822          *in43=*on    ;
049000130822          $err='1'     ;
049100130822          v1cmsg = Msg(08);
049200130822          leavesr;
049300130822 1     endif  ;
049400130822
049500130822       // orario minimo <= all'inizio
049600130822       if v1romiis>v1rostis  ;
049700130822          *in90=*on    ;
049800130822          *in28=*on    ;
049900130822          *in42=*on    ;
050000130822          $err='1'     ;
050100130822          v1cmsg = Msg(09);
050200130822          leavesr;
050300130822       endif  ;
050400130822
050500130822       // orario massimo >= alla fine
050600130822       if v1romxfs<v1rostfs  ;
050700130822          *in90=*on    ;
050800130822          *in28=*on    ;
050900130822          *in43=*on    ;
051000130822          $err='1'     ;
051100130822          v1cmsg = Msg(11);
051200130822          leavesr;
051300130822       endif  ;
051400130822
051500130822
051600130822       // ORARI :  Consegne
051700130822       if  v1costis=0  ;
051800130822          *in90=*on    ;
051900130822          *in28=*on    ;
052000130822          *in45=*on    ;
052100130822          $err='1'     ;
052200130822          v1cmsg = Msg(07);
052300130822          leavesr;
052400130822 1     endif  ;
052500130822
052600130822       wora=v1costis ;
052700130822       exsr contr_ora ;
052800130822       if v1cmsg<>*blanks ;
052900130822          *in90=*on    ;
053000130822          *in28=*on    ;
053100130822          *in45=*on    ;
053200130822          $err='1'     ;
053300130822          leavesr;
053400130822 1     endif  ;
053500130822
053600130822 1     if  v1costfs=0     ;
053700130822          *in90=*on    ;
053800130822          *in28=*on    ;
053900130822          *in46=*on    ;
054000130822          $err='1'     ;
054100130822          v1cmsg = Msg(07);
054200130822          leavesr;
054300130822 1     endif  ;
054400130822
054500130822       wora=v1costfs ;
054600130822       exsr contr_ora ;
054700130822       if v1cmsg<>*blanks ;
054800130822          *in90=*on    ;
054900130822          *in28=*on    ;
055000130822          *in46=*on    ;
055100130822          $err='1'     ;
055200130822          leavesr;
055300130822 1     endif  ;
055400130822
055500130822 1     if  v1costfs<v1costis  ;
055600130822          *in90=*on    ;
055700130822          *in28=*on    ;
055800130822          *in46=*on    ;
055900130822          $err='1'     ;
056000130822          v1cmsg = Msg(08);
056100130822          leavesr;
056200130822 1     endif  ;
056300130822
056400130822 1     if  v1comiis=0     ;
056500130822          *in90=*on    ;
056600130822          *in28=*on    ;
056700130822          *in47=*on    ;
056800130822          $err='1'     ;
056900130822          v1cmsg = Msg(07);
057000130822          leavesr;
057100130822 1     endif  ;
057200130822
057300130822       wora=v1comiis ;
057400130822       exsr contr_ora ;
057500130822       if v1cmsg<>*blanks ;
057600130822          *in90=*on    ;
057700130822          *in28=*on    ;
057800130822          *in47=*on    ;
057900130822          $err='1'     ;
058000130822          leavesr;
058100130822 1     endif  ;
058200130822 1     if  v1comxfs=0     ;
058300130822          *in90=*on    ;
058400130822          *in28=*on    ;
058500130822          *in48=*on    ;
058600130822          $err='1'     ;
058700130822          v1cmsg = Msg(07);
058800130822          leavesr;
058900130822 1     endif  ;
059000130822
059100130822       wora=v1comxfs ;
059200130822       exsr contr_ora ;
059300130822       if v1cmsg<>*blanks ;
059400130822          *in90=*on    ;
059500130822          *in28=*on    ;
059600130822          *in48=*on    ;
059700130822          $err='1'     ;
059800130822          leavesr;
059900130822 1     endif  ;
060000130822
060100130822 1     if  v1comxfs<v1comiis  ;
060200130822          *in90=*on    ;
060300130822          *in28=*on    ;
060400130822          *in48=*on    ;
060500130822          $err='1'     ;
060600130822          v1cmsg = Msg(08);
060700130822          leavesr;
060800130822 1     endif  ;
060900130822
061000130822       // orario minimo <= all'inizio
061100130822       if v1comiis>v1costis  ;
061200130822          *in90=*on    ;
061300130822          *in28=*on    ;
061400130822          *in47=*on    ;
061500130822          $err='1'     ;
061600130822          v1cmsg = Msg(09);
061700130822          leavesr;
061800130822       endif  ;
061900130822
062000130822       // orario massimo >= alla fine
062100130822       if v1comxfs<v1costfs  ;
062200130822          *in90=*on    ;
062300130822          *in28=*on    ;
062400130822          *in48=*on    ;
062500130822          $err='1'     ;
062600130822          v1cmsg = Msg(11);
062700130822          leavesr;
062800130822       endif  ;
062900130822      /end-free
063000130822
063100000000     C*
063200000000     C     finct1        ENDSR
063300130822      /free
063400130822       //--------------------------------------------------------------
063500130822       //?Controllo campo ORA
063600130822       //--------------------------------------------------------------
063700130822       BEGSR Contr_ORA ;
063800130822       if whh < 0  or whh > 24  ;
063900130822          v1cmsg = Msg(07);
064000130822       endif  ;
064100130822       if wmm < 0  or wmm > 59  ;
064200130822          v1cmsg = Msg(07);
064300130822       endif  ;
064400130822
064500130822       ENDSR ;
064600130822      /end-free
064700130822
064800000000     c*--------------------------------------------------------------------------------------------*
064900000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
065000000000     c*--------------------------------------------------------------------------------------------*
065100000000     C     f03ges        BEGSR
065200000000     C*
065300000000     C                   MOVEL     'S'           $fine                          *fine programma
065400000000     C*
065500000000     C                   ENDSR
065600000000     c*--------------------------------------------------------------------------------------------*
065700000000     C* f06ges - gestione tasto funzionale f06 ==> Conferma
065800000000     c*--------------------------------------------------------------------------------------------*
065900000000     c     f06ges        BEGSR
066000130823     c
066100130823     c                   clear                   conta             9 0
066200130823     c                   clear                   deletati          9 0
066300130823     c                   open      qsysprt
066400130823     c                   seton                                        OF
066500000000     c*
066600130822     c* elabora AZCPC --> scrittura AZCOS00F
066700130822     c                   if        v1ctfile='C' or v1ctfile='E'
066800130823     c                   eval      wfile='C'
066900130822     c                   exsr      Leggi_azCPC
067000130822     c                   endif
067100130822     c                   if        v1ctfile='L' or v1ctfile='E'
067200130823     c                   eval      wfile='L'
067300130823     c                   exsr      Leggi_azCPL
067400130822     c                   endif
067500130823
067600130823     c                   if        v1csim='S'
067700130823     c                   if        *inof
067800130823     c                   except    tes
067900130823     c                   endif
068000130823     c                   except    totale
068100130823     c                   endif
068200130823     c
068300130823     c                   close     qsysprt
068400000000     c* invia messaggio
068500130822     c                   SETON                                        9028
068600130822     c                   MOVEL     msg(10)       v1cmsg
068700130823     c                   if        v1csim='N'
068800130823     c                   eval      v1cmsg=%trim(v1cmsg)+' scritti '+
068900130823     c                             %editc(conta:'2')
069000130823     c                   if        deletati>0
069100130823     c                   eval      v1cmsg=%trim(v1cmsg)+ ';cancellati ' +
069200130823     c                             %editc(deletati:'2')
069300130823     c                   endif
069400130823     c                   eval      v1cmsg=%trim(v1cmsg)+ ' record...'
069500130823     c
069600130823     c                   else
069700130823     c                   eval      v1cmsg=%trim(v1cmsg)+' creata stampa...'
069800130823     c                   endif
069900000000     c*
070000000000     c                   ENDSR
070100000000     c*--------------------------------------------------------------------------------------------*
070200000000     C* f10ges - gestione tasto funzionale f10 ==> Ricerca versioni
070300000000     c*--------------------------------------------------------------------------------------------*
070400000000     C     f10ges        BEGSR
070500000000     c*
070600000000     c* lancia il programma di gestione versioni in ricerca
070700000000     c                   CLEAR                   dssi00                         *pulisce ds
070800000000     c                   MOVEL     'R01'         d00op0                         *livello procedura
070900000000     c                   MOVEL     *blanks       d00op1                         *livello programma
071000000000     c                   MOVEL     '0'           d00f03                         *premuto F03
071100000000     c                   MOVEL     '0'           d00f12                         *premuto F12
071200000000     c                   MOVEL     '0'           d00err                         *errore
071300000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
071400000000     c                   CALL      'TISI46R'                                    *gestione versioni
071500000000     c                   PARM                    kpjba
071600000000     c                   PARM                    dssi00
071700000000if  1c                   IF        d00err='1'                                   *errore
071800000000     C                   SETON                                        2190
071900130822     C                   MOVEL     msg(2)        v1cmsg
072000000000x   1c                   ELSE                                                   *no errore
072100000000     c                   Z-ADD     d00ver        v1cver
072200000000e   1c                   ENDIF
072300000000     c                   MOVEL     '1'           $tesvd
072400000000     C*
072500000000     C                   ENDSR
072600000000     c*--------------------------------------------------------------------------------------------*
072700000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
072800000000     c*--------------------------------------------------------------------------------------------*
072900000000     C     f12ges        BEGSR
073000000000     C*
073100000000     C                   MOVEL     'S'           $fine                          *fine programma
073200000000     C*
073300000726     C                   ENDSR
073400130822     c*--------------------------------------------------------------------------------------------*
073500130822     C* Lettura file AZCPC00F
073600130822     c*--------------------------------------------------------------------------------------------*
073700130822     C     Leggi_AZCPC   BEGSR
073800130822
073900130823     c* Se immesso cap dal / al, eseguo sql per questo e poi parzializzo per
074000130822     c*  linea se immessa
074100130822
074200130822      /free
074300130822
074400130823       if  v1ccapd<>*blanks ;
074500130822        wrkgetlista ='select * from azcpc00f where cpcver='  +
074600130823                       %char(v1cver) +' and cpcnar=''   '' and cpccap>='''+
074700130823                       v1ccapd + ''' and cpccap<=''' + v1ccapa + ''''  ;
074800130822
074900130823       kdlna='Std  '  ;
075000130823       exsr AZCPC_Sql    ;
075100130823       leavesr  ;
075200130823       endif  ;
075300130823
075400130823       if comlna>0 and v1ctlna=' ' or v1clna='S' or v1clna='E'  ;
075500130823
075600130823        wrkgetlista ='select * from azcpc00f where cpcver='  +
075700130823                       %char(v1cver) +' and cpcnar=''   '' and cpclna =  '+
075800130823                       %char(comlna)  ;
075900130823       exsr AZCPC_Sql    ;
076000130823       endif  ;
076100130823
076200130823       if comlna>0 and v1ctlna=' ' or v1clna='O' or v1clna='E'  ;
076300130823        wrkgetlista ='select * from azcpc00f where cpcver='  +
076400130823                       %char(v1cver) +' and cpcnar=''   '' and cpclol =  '+
076500130823                       %char(comlna)  ;
076600130823       kdlna='Oltre'  ;
076700130823       exsr AZCPC_Sql    ;
076800130823       endif  ;
076900130823       if comlna>0 and v1ctlna=' ' or v1clna='T' or v1clna='E'  ;
077000130823        wrkgetlista ='select * from azcpc00f where cpcver='  +
077100130823                       %char(v1cver) +' and cpcnar=''   '' and cpclos =  '+
077200130823                       %char(comlna)  ;
077300130823       kdlna='Sotto'  ;
077400130823       exsr AZCPC_Sql    ;
077500130823       endif  ;
077600130823
077700130823       ENDSR  ;
077800130823
077900130823       // -----------------------------------------------------------------------------------------*
078000130823       BEGSR AZCPC_sql  ;
078100130823       // -----------------------------------------------------------------------------------------*
078200130823
078300130822       EXEC sql  prepare s1 from : wrkgetlista ;
078400130822       EXEC sql  declare a1 cursor for s1      ;
078500130822       EXEC sql  open    a1                    ;
078600130822
078700130822       EXEC sql fetch next from a1 into : azcpcds  ;
078800130822
078900130822       dow sqlcod=0    ;
079000130822
079100130822       clear welab  ;
079200130822
079300130822       // Se parzializzato per linea di arrivo  --> la controllo
079400130823       if comlna>*zeros  and v1ccapd<>*blanks  ;
079500130823       if v1ctlna=' ' or v1ctlna='E'  ;
079600130822       if comlna<>cpclna and comlna<>cpclol and comlna<>cpclos  ;
079700130822        welab='N'  ;
079800130822       endif  ;
079900130823       endif  ;
080000130823       if v1ctlna='S' and comlna<>cpclna ;
080100130823        welab='N'  ;
080200130823       endif  ;
080300130823       if v1ctlna='O' and comlna<>cpclol ;
080400130823        welab='N'  ;
080500130823       endif  ;
080600130823       if v1ctlna='T' and comlna<>cpclos ;
080700130823        welab='N'  ;
080800130823       endif  ;
080900130823       select ;
081000130823       when comlna = cpclna  ;
081100130823       kdlna='Std  ';
081200130823
081300130823       when comlna = cpclol  ;
081400130823       kdlna='Oltre';
081500130823
081600130823       when comlna = cpclos  ;
081700130823       kdlna='Sotto';
081800130823       endsl  ;
081900130823
082000130822       endif  ;
082100130822
082200130822       if Welab<>'N' ;
082300130822       exsr crea_azcos   ;
082400130822       endif  ;
082500130822
082600130822       EXEC sql fetch next from a1 into : azcpcds  ;
082700130822       enddo           ;
082800130822
082900130822       EXEC sql  CLOSE   a1                    ;
083000130823
083100130823
083200130823       ENDSR   ;
083300130822
083400130823       //------------------------------------------------------------------------------------------*
083500130823       //  Lettura file AZCPL00F
083600130823       //------------------------------------------------------------------------------------------*
083700130823       BEGSR    Leggi_AZCPL   ;
083800130823
083900130823        // Se immesso cap dal / al, eseguo sql per questo e poi parzializzo per
084000130823        //  linea se immessa
084100130823
084200130823       if  v1ccapd<>*blanks ;
084300130823        wrkgetlista ='select * from azcpl00f where cplver='  +
084400130823                       %char(v1cver) +' and cplnar=''   '' and cplcap>='''+
084500130823                       v1ccapd + ''' and cplcap<=''' + v1ccapa + ''''  ;
084600130823
084700130823       exsr AZCPL_Sql    ;
084800130823       leavesr  ;
084900130823       endif  ;
085000130823
085100130823       if comlna>0 and v1ctlna=' ' or v1clna='S' or v1clna='E'  ;
085200130823
085300130823        wrkgetlista ='select * from azcpl00f where cplver='  +
085400130823                       %char(v1cver) +' and cplnar=''   '' and cpllna =  '+
085500130823                       %char(comlna)  ;
085600130823       kdlna='Std  '  ;
085700130823       exsr AZCPL_Sql    ;
085800130823       endif  ;
085900130823
086000130823       if comlna>0 and v1ctlna=' ' or v1clna='O' or v1clna='E'  ;
086100130823        wrkgetlista ='select * from azcpl00f where cplver='  +
086200130823                       %char(v1cver) +' and cplnar=''   '' and cpllol =  '+
086300130823                       %char(comlna)  ;
086400130823       kdlna='Oltre'  ;
086500130823       exsr AZCPL_Sql    ;
086600130823       endif  ;
086700130823       if comlna>0 and v1ctlna=' ' or v1clna='T' or v1clna='E'  ;
086800130823        wrkgetlista ='select * from azcpl00f where cplver='  +
086900130823                       %char(v1cver) +' and cplnar=''   '' and cpllos =  '+
087000130823                       %char(comlna)  ;
087100130823       kdlna='Sotto'  ;
087200130823       exsr AZCPL_Sql    ;
087300130823       endif  ;
087400130823
087500130823       ENDSR  ;
087600130823
087700130823       // -----------------------------------------------------------------------------------------*
087800130823       BEGSR AZCPL_sql  ;
087900130823       // -----------------------------------------------------------------------------------------*
088000130823
088100130823       EXEC sql  prepare s2 from : wrkgetlista ;
088200130823       EXEC sql  declare a2 cursor for s2      ;
088300130823       EXEC sql  open    a2                    ;
088400130823
088500130823       EXEC sql fetch next from a2 into : azcplds  ;
088600130823
088700130823       dow sqlcod=0    ;
088800130823
088900130823       clear welab  ;
089000130823
089100130823       // Se parzializzato per linea di arrivo  --> la controllo
089200130823       if comlna>*zeros  and v1ccapd<>*blanks  ;
089300130823       if v1ctlna=' ' or v1ctlna='E'  ;
089400130823       if comlna<>cpllna and comlna<>cpllol and comlna<>cpllos  ;
089500130823        welab='N'  ;
089600130823       endif  ;
089700130823       endif  ;
089800130823       if v1ctlna='S' and comlna<>cpllna ;
089900130823        welab='N'  ;
090000130823       endif  ;
090100130823       if v1ctlna='O' and comlna<>cpllol ;
090200130823        welab='N'  ;
090300130823       endif  ;
090400130823       if v1ctlna='T' and comlna<>cpllos ;
090500130823        welab='N'  ;
090600130823       endif  ;
090700130823       select ;
090800130823       when comlna = cpllna  ;
090900130823       kdlna='Std  ';
091000130823
091100130823       when comlna = cpllol  ;
091200130823       kdlna='Oltre';
091300130823
091400130823       when comlna = cpllos  ;
091500130823       kdlna='Sotto';
091600130823       endsl  ;
091700130823       endif  ;
091800130823
091900130823       if Welab<>'N' ;
092000130823       exsr crea_azcos   ;
092100130823       endif  ;
092200130823
092300130823       EXEC sql fetch next from a2 into : azcplds  ;
092400130823       enddo           ;
092500130823
092600130823       EXEC sql  CLOSE   a2                    ;
092700130823       ENDSR   ;
092800130823
092900130822      /end-free
093000130823
093100000000     c*--------------------------------------------------------------------------------------------*
093200130822     c* Creazione file AZCOS
093300000000     c*--------------------------------------------------------------------------------------------*
093400130822     c     crea_Azcos    BEGSR
093500130822
093600130822     c* cerco nel file se trovo già il record
093700130823     c                   if        wfile='C'
093800130822     c                   eval      knar=cpcnar
093900130822     c                   eval      kcap=cpccap
094000130822     c                   clear                   kloc
094100130823     c                   else
094200130823     c                   eval      knar=cplnar
094300130823     c                   eval      kcap=cplcap
094400130823     c                   eval      kloc=cplloc
094500130823     c                   endif
094600130823     c
094700130822     c                   if        comlna>0
094800130822     c                   eval      klna=comlna
094900130822     c                   eval      ktser='R'
095000130822     c                   exsr      write_cos
095100130822     c                   eval      ktser='C'
095200130822     c                   exsr      write_cos
095300130822     c                   else
095400130822     c
095500130822     c                   if        v1ctlna='S' or v1ctlna='E'
095600130823     c                   if        wfile='C'
095700130822     c                   eval      klna=cpclna
095800130823     c                   else
095900130823     c                   eval      klna=cpllna
096000130823     c                   endif
096100130822     c                   eval      ktser='R'
096200130822     c                   exsr      write_cos
096300130822     c                   eval      ktser='C'
096400130822     c                   exsr      write_cos
096500130822     c                   endif
096600130823     c                   if        v1ctlna='O' or v1ctlna='E'
096700130823     c                   if        wfile='C'
096800130822     c                   eval      klna=cpclol
096900130823     c                   else
097000130823     c                   eval      klna=cpllol
097100130823     c                   endif
097200130823     c                   if        klna>0
097300130822     c                   eval      ktser='R'
097400130822     c                   exsr      write_cos
097500130822     c                   eval      ktser='C'
097600130822     c                   exsr      write_cos
097700130822     c                   endif
097800130823
097900130823     c                   endif
098000130823     c                   if        v1ctlna='T' or v1ctlna='E'
098100130823     c                   if        wfile='C'
098200130822     c                   eval      klna=cpclos
098300130823     c                   else
098400130823     c                   eval      klna=cpllos
098500130823     c                   endif
098600130823     c                   if        klna>0
098700130822     c                   eval      ktser='R'
098800130822     c                   exsr      write_cos
098900130822     c                   eval      ktser='C'
099000130822     c                   exsr      write_cos
099100130823     c                   endif
099200130823     c
099300130822     c                   endif
099400130822     c                   endif
099500130822     c
099600130822     c                   ENDSR
099700130822     c*--------------------------------------------------------------------------------------------*
099800130822     c* Scrittura del file o crezione record di stampa
099900130822     c*--------------------------------------------------------------------------------------------*
100000130822     c     write_cos     BEGSR
100100130823     c                   setoff                                       03
100200130830     c                   clear                   kkloc
100300130830     c                   clear                   kkcap
100400130830     c
100500130830     c* chaino il record per la lna
100600130830     c
100700130823     c
100800130822     c     kcos          chain     azcos01l
100900130822     c* Se esiste già e devo andare in agginta soltanto -->  esco
101000130830    1c                   if        v1caggio='W' and %found(azcos01l)
101100130822     c                   leavesr
101200130830    1c                   endif
101300130822     c
101400130822     c                   clear                   azcos000
101500130822     c                   eval      cosver=v1cver
101600130822     c                   eval      coslna=klna
101700130822     c                   eval      coscap=kcap
101800130822     c                   eval      cosloc=kloc
101900130822     c                   eval      cosnar=knar
102000130822     c* ritiri
102100130830    1c                   if        ktser='R'
102200130822     c                   eval      costser='R'
102300130822     c                   eval      cosostis= v1rostis
102400130822     c                   eval      cosostfs= v1rostfs
102500130822     c                   eval      cosomiis= v1romiis
102600130822     c                   eval      cosomxfs= v1romxfs
102700130822     c                   eval      costrzs = v1rtrzs
102800130830   x1c                   else
102900130822     c                   eval      costser='C'
103000130822     c                   eval      cosostis= v1costis
103100130822     c                   eval      cosostfs= v1costfs
103200130822     c                   eval      cosomiis= v1comiis
103300130822     c                   eval      cosomxfs= v1comxfs
103400130822     c                   eval      costrzs = v1ctrzs
103500130830    1c                   endif
103600130830     c
103700130830     c                   movel     azcosds       azcossav
103800130830     c
103900130830    1c                   if        kloc<>*blanks
104000130830     c
104100130830     c                   eval      kkcap=kcap
104200130830     c     kcoslna       chain(N)  azcos01l
104300130830    1c                   endif
104400130830
104500130830    1c                   if        kloc=*blanks or not %found(azcos01l)
104600130830     c                   clear                   kkcap
104700130830     c
104800130830     c     kcoslna       chain(N)  azcos01l
104900130830    1c                   endif
105000130830     c
105100130830    1c                   if        %found(azcos01l)
105200130830     c                   clear                   cosute
105300130830     c                   clear                   cosduv
105400130830     c                   clear                   s_cosloc
105500130830     c
105600130830    2c                   if        kkcap=*blanks
105700130830     c                   clear                   s_coscap
105800130830    2c                   endif
105900130830     c
106000130830    2c                   if        azcosDS=azcosSAV
106100130830     c                   seton                                        03
106200130830    2c                   endif
106300130830    1c                   endif
106400130830     c
106500130823     c* Richaino il rec precedente
106600130823     c     kcos          chain     azcos01l
106700130823     c                   eval      azcosDS=azcossav
106800130830     c                   eval      cosloc=kloc
106900130830     c                   eval      coscap=kcap
107000130823     c
107100130822     c                   eval      cosute  = knmus
107200130822     c                   movel     datasys       cosduv
107300130823     c
107400130822     c                   if        v1csim='N'
107500130823     c                   if        not *in03
107600130823
107700130822     c                   if        not %found(azcos01l)
107800130822     c                   write     azcos000
107900130822     c                   else
108000130822     c                   update    azcos000
108100130822     c                   endif
108200130823     c
108300130823     c                   else
108400130823     c
108500130830     c* Se c'era il record ma diventa uguale a quello del CAP/fil lo deleto
108600130823     c                   if        %found(azcos01l)
108700130823     c                   add       1             deletati
108800130823     c                   delete    azcos000
108900130823     c                   endif
109000130823     c
109100130823     c                   endif
109200130822     c
109300130822     c                   else
109400130822     c                   exsr      stadet
109500130822     c                   endif
109600130823     c
109700130823     c  n03              add       1             conta
109800130822     c
109900130822     c                   ENDSR
110000130822     c*--------------------------------------------------------------------------------------------*
110100130822     c* stadet - stampa esito elaborazione
110200130822     c*--------------------------------------------------------------------------------------------*
110300130822     c     stadet        BEGSR
110400000000     c*
110500000000     c* stampa testata
110600000000if  1c                   IF        *inof                                        *fine pagina
110700000000     c                   EXCEPT    tes
110800000000     c                   MOVEL     '0'           *inof
110900000000e   1c                   ENDIF
111000000000     c*
111100000000     c* stampa dettaglio
111200130823     c                   if        *in03
111300130823     c                   if        %found(azcos01l)
111400130823     c                   add       1             deletati
111500130830     c                   if        kkcap<>*blanks
111600130823     c                   eval      staesito=msp(4)
111700130830     c                   else
111800130830     c                   eval      staesito=msp(6)
111900130830     c                   endif
112000130830     c
112100130823     c                   else
112200130830     c                   if        kkcap<>*blanks
112300130823     c                   eval      staesito=msp(3)
112400130830     c                   else
112500130830     c                   eval      staesito=msp(5)
112600130830     c                   endif
112700130823     c                   endif
112800130823     c                   else
112900130823     c
113000130822     c                   if        not %found(azcos01l)
113100130822     c                   eval      staesito=msp(2)
113200130822     c                   else
113300130822     c                   eval      staesito=msp(1)
113400130822     c                   endif
113500130823     c                   endif
113600130822
113700000000     c                   EXCEPT    det
113800000000     c*
113900000000     c                   ENDSR
114000000000     c*--------------------------------------------------------------------------------------------*
114100000000     C* impvd0 - Impostazioni testata
114200000000     c*--------------------------------------------------------------------------------------------*
114300000000     C     impvd0        BEGSR
114400000000     C*
114500000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
114600000000     C*
114700000000     C                   ENDSR
114800000000     c*--------------------------------------------------------------------------------------------*
114900000000     C* finsr - operazioni finali
115000000000     c*--------------------------------------------------------------------------------------------*
115100000000     C     finsr         BEGSR
115200000000     C*
115300000000     C                   ENDSR
115400000000     c*--------------------------------------------------------------------------------------------*
115500000000     C* *inzsr - operazioni iniziali
115600000000     c*--------------------------------------------------------------------------------------------*
115700000000     C     *inzsr        BEGSR
115800000000     c*
115900000000     c* ricevimento parametri
116000000000     c     *ENTRY        PLIST
116100000000     c                   PARM                    kpjba
116200130823     c
116300130823     c                   seton                                        OF
116400000000     C*
116500000000     C* chiavi di lettura
116600130822     C     kCOS          klist
116700130822     C                   KFLD                    v1cver
116800130822     C                   KFLD                    knar                            -nazione
116900130822     C                   KFLD                    kcap                            -cap
117000130822     C                   KFLD                    kloc                            -località
117100130822     C                   KFLD                    klna
117200130823     C                   KFLD                    ktser
117300130830     C     kCOSlna       klist
117400130830     C                   KFLD                    v1cver
117500130830     C                   KFLD                    knar                            -nazione
117600130830     C                   KFLD                    kkcap                           -cap
117700130830     C                   KFLD                    kkloc                           -località
117800130830     C                   KFLD                    klna
117900130830     C                   KFLD                    ktser
118000000000     C*
118100000000     C* reperisce la data corrente
118200000000     C                   TIME                    n14                            *ora (6)+ data (8)
118300000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
118400000000     C                   Z-ADD     n8            datcor                         *Data corrente
118500000000     c*
118600000000     c                   ENDSR
118700000000     o*--------------------------------------------------------------------------------------------*
118800000000     o* Stampa
118900000000     o*--------------------------------------------------------------------------------------------*
119000000000     Oqsysprt   E            tes              02
119100000000     O                       sta(1)              66
119200000000     O                       sta(2)             132
119300000000     O                       datcor        8    117
119400000000     O                       PAGE          Z    132
119500000000     O*------------------------
119600000000     O          E            tes              03
119700000000     O                       sta(3)              66
119800000000     O                       sta(3)             132
119900000000     O*------------------------
120000000000     O          E            tes              04
120100130823     O                       sta(8)              66
120200130823     O                       sta(9)             132
120300130822     o                       v1rostis            27 '0 :  '
120400130822     o                       v1rostfs            33 '0 :  '
120500130822     o                       v1romiis            44 '0 :  '
120600130822     o                       v1romxfs            55 '0 :  '
120700130822     o                       v1rtrzs             65
120800130822     o                       v1costis            93 '0 :  '
120900130822     o                       v1costfs            99 '0 :  '
121000130822     o                       v1comiis           110 '0 :  '
121100130822     o                       v1comxfs           121 '0 :  '
121200130822     o                       v1ctrzs       z    131
121300130823     O*------------------------
121400130823     O          E            tes              05
121500130823     O                       sta(4)              66
121600130823     O                       sta(6)             132
121700000000     O*------------------------
121800130823     O          E            tes              06
121900000000     O                       sta(3)              66
122000000000     O                       sta(3)             132
122100000000     O*------------------------
122200000000     O          E            det         1
122300130822     O                       v1cver        Z      5
122400130822     O                       knar             +   1
122500130822     O                       kcap             +   1
122600130822     O                       kloc             +   1
122700130822     O                       klna             +   1
122800130823     O                       kdlna            +   1
122900130823     O                       ktser            +   2
123000130823     O                       staesito         +   5
123100130823     O          E            totale      1
123200130823     o                                              'Totale da scrivere :'
123300130823     O                       conta         Z  +   2
123400130823     o                                        +   2 'Totale da cancellare:'
123500130823     O                       deletati      Z  +   2
123600000000     O*--------------------------------------------------------------------------------------------*
123700000000** MSG -Messaggi video-
123800130822Immettere uno dei valori possibili
123900130822Versione inesistente
124000130822Immettere sia C.A.P DAL che C.A.P AL
124100130823Il C.A.P   DAL   deve essere MINORE del C.A.P   AL
124200130822Linea di arrivo errata
124300130822La linea di arrivio non può essere estera
124400130822Immettere un orario formalmente corretto                                                  7
124500130822L'orario di fine deve essere maggiore dell'orario iniziale                                8
124600130822L'orario minimo non può essere maggiore dell'orario di inizio standard                    9
124700130823Elaborazione eseguita:
124800130822L'orario massimo non può essere minore dell'orario di fine standard                      11
124900130822Se immessa la linea di arrivo, NON indicare il tipo linea da elaborare                   12
125000130829Immettere la linea di arrivo da aggiornare                                               13
125100130823C.A.P. formalmente errato                                                                14
125200130823Immettere i CAP dal/al o la linea di arrivo come parzializzazione                        15
125300000000** STA -Righe di stampa -
125400130823TISIO3R     SIMULAZIONE          ***  Crea orari servizi Cappario 1
125500130822 AZCOS00F  ***                           99/99/99         PAG.99992
125600000000------------------------------------------------------------------3
125700130823VER   NAZ CAP       LOCALITA'                           lna       4
12580013082399999 XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXX xxxxx 5
125900130823 Serv   ESITO
126000130823  x     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
126100130822  Orari ritiro  : Std xx:xx-xx:xx  Min xx:xx  Max xx:xx  Reaz. xx 8
126200130822  Orari consegna: Std xx:xx-xx:xx  Min xx:xx  Max xx:xx  Reaz. xx 9
12630000000010
126400000000** MSP -Messaggi stampa-
126500130822Già ESISTENTE
126600130822Nuova scrittura
126700130830Dati  UGUALI al CAP: NO SCRITTURA
126800130830Dati  UGUALI al CAP: CANCELLATO
126900130830Dati  UGUALI alla FILIALE: NO SCRITTURA
127000130830Dati  UGUALI ALLA filiale: CANCELLATO
1271000000005
127200130823**   NUM
1273001308230123456789
