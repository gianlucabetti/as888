000100970609     h*--------------------------------------------------------------------------------------------*
000200970610     h* Allineamento organigramma clienti - Esegue
000300970609     h*--------------------------------------------------------------------------------------------*
000400970609     h DECEDIT('0,') DATEDIT(*DMY.)
000500970609     f*--------------------------------------------------------------------------------------------*
000600980310     f* Data base
000700970609     f*--------------------------------------------------------------------------------------------*
000800980310     fazorg01l  IF   E           K DISK                                         *organigramma
000900970609     fazcpc01l  IF   E           K DISK                                         *cappario atturale
001000970609     fazcap00x  O  A E           K DISK                                         *cappario vecchio
001100970609     f                                     RENAME(azcap000:azcapx)
001200970609     fazist00f  O  A E           K DISK                                         *cappario vecchio
001300970609     f                                     RENAME(azist:azist0)
001400970609     f                                     PREFIX(a_)
001500970609     fazist10f  O  A E           K DISK                                         *cappario vecchio
001600970609     f                                     RENAME(azist:azist1)
001700970609     f                                     PREFIX(b_)
001800970609     fqsysprt   O    F  132        PRINTER OFLIND(*inof)                        *Printer file
001900970609     f                                     FORMLEN(66) FORMOFL(60)
002000970609     d*--------------------------------------------------------------------------------------------*
002100970609     d* Data structure
002200970609     d*--------------------------------------------------------------------------------------------*
002300970609     d*----------------
002400970609     d* Schiere
002500970609     d*----------------
002600970609     d slin            S             66    DIM(10) CTDATA PERRCD(1)             *righe di stampa
002700980310     d org             S              3  0 DIM(500)                             *filiali
002800980310     d odit            S              3    DIM(500)                             *ditta filiali
002900970609     d*----------------
003000970609     d* Variabili riferite al data base
003100970609     d*----------------
003200970618     d kccver          S                   LIKE(cpcver)                         *per azcpl00f
003300970609     d*----------------
003400970609     d* Variabili di controllo
003500970609     d*----------------
003600970609     d $fine           S              1    INZ('N')                             *fine programma
003700970609     d $recok          S              1    INZ('N')                             *validità record
003800970609     d $err            S              1    INZ('0')                             *errore generico
003900970609     d*----------------
004000970609     d* Variabili di programma
004100970609     d*----------------
004200980310     d i               S              5  0                                      *indice
004300980310     d wn14            S             14  0                                      *Numerico 14
004400970609     d wn8             S              8  0                                      *Numerico 8
004500970609     d wa1             S              1                                         *Alfanumerico 1
004600970609     d wa5             S              5                                         *Alfanumerico 5
004700970722     d wn115           S             11  5                                      *Numerico 11,5
004800971020     d datcor          S              8  0                                      *data corrente
004900980310     d wlin            S                   LIKE(cpclna)                         *linea arrivo
005000980310     d wzon            S                   LIKE(cpcznc)                         *zona consegna
005100970609     d*----------------
005200970609     d* Ds
005300970609     d*----------------
005400971020     d wlbda8          DS                  INZ                                  *controlla data
005500971020     d  g08dat                        8  0                                       -data dritta
005600971020     d  g08inv                        8  0                                       -data invertita
005700971020     d  g08err                        1                                          -errore
005800971020     d  g08tgi                        5  0                                       -giorni fra date
005900970707     d p96dri          S              8  0                                      tisi96R-data riferim
006000970707     d p96ver          S              5  0                                      tisi96R-versione
006100020919     D TIBS34DS      E DS                                                       *Profili utente
006200020919     D DDATIUTE      E DS                                                       *Dati utente
006300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006400970609     d dspar50         DS                                                       *ds parametri lancio
006500970618     d  p50dri                        8                                          -data riferimento
006600970618     d  p50fil                        3                                          -filiale
006700970609     d  p50fmr                        1                                          -tracciato record
006800970609     d  p50sac                        1                                          -solo agg.cappario
006900010703     d  p50bos                        1                                          -clienti BAR o SDI
007000010918     d  p50azz                        1                                          -azzeramento linee
007100020805     d tibs55ds      E DS
007200970609     d dssi95        E DS                  EXTNAME(tisi95ds)                    *ds "TISI95R"
007300970609     d kpjba         E DS                                                       *architettura
007400970609     c*--------------------------------------------------------------------------------------------*
007500970609     c* Main lines
007600970609     c*--------------------------------------------------------------------------------------------*
007700970618     C*
007800970618     C* Imposta le variabili di lavoro in base ai parametri di entrata
007900970618     C                   EXSR      imppar
008000970609     C*
008100970609     C* Posizionamento sul file e prima lettura
008200970609if  1c                   IF        $err='0'                                     *no errori
008300970609     C                   EXSR      setfil
008400970609     C*
008500970609     C* ciclo fino a fine file
008600970609do  2C                   DOW       $fine='N'
008700970609     C*
008800970609     C* controlla il cap
008900970609     C                   EXSR      chkcap
009000970609     C*
009100970609     C* scrive il cap sul file di output
009200970609if  3c                   IF        $err='0'                                     *no errori
009300970609     C                   EXSR      scrist0
009400970609     C*
009500970609     C* scrive il cap sul file di output
009600970609     C                   EXSR      scrist1
009700970609     C*
009800970609     C* scrive il cap sul file di output
009900970609     C                   EXSR      scrcapx
010000970609e   3c                   ENDIF
010100970609     C*
010200970609     C* lettura sucessiva record
010300970610     C                   EXSR      letfil
010400970609     C*
010500970609e   2C                   ENDDO                                                  *fine cap
010600970609e   1c                   ENDIF
010700970609     C*
010800970609     C* operazioni finali
010900970609     c                   EXSR      finsr
011000970609     C* fine pgm
011100970609     C                   SETON                                            lr
011200970609     c*--------------------------------------------------------------------------------------------*
011300970609     c* setfil - posizionamento sul file e prima lettura
011400970609     c*--------------------------------------------------------------------------------------------*
011500970609     C     setfil        BEGSR
011600970609     C*
011700970609     C* posizionamento sul file
011800970707     c                   Z-ADD     p96ver        kccver
011900970609     c     keycpc        SETLL     azcpc01l                           99
012000970609if  1c                   IF        NOT *in99
012100970609     c                   EXSR      letfil                                       *lettura record
012200970609x   1c                   ELSE
012300970609     C                   MOVEL     'S'           $fine                          *fine programma
012400970609e   1C                   ENDiF
012500970609     C*
012600970609     C                   ENDSR
012700970609     c*--------------------------------------------------------------------------------------------*
012800970609     c* letfil - lettura record
012900970609     c*--------------------------------------------------------------------------------------------*
013000970609     C     letfil        BEGSR
013100970609     C*
013200970609     C* Legge fino a:
013300970609     C*       -fine file
013400970609     C*       -trovato record valido
013500970609     c                   MOVEL     'N'           $recok                         *record non valido
013600970609do  1c                   DOW       NOT *in99 AND
013700970609     c                             $recok='N'
013800970609     C     keycpc        READE     azcpc01l                               99
013900970609if  2c                   IF        *in99                                        *fine file
014000970609     c                   MOVEL     'S'           $fine                          *fine programma
014100970609x   2c                   ELSE                                                   *record trovato
014200970609     c                   EXSR      chkrec                                       *controlla record
014300970609e   2c                   ENDIF
014400970609e   1c                   ENDDO
014500970609     C*
014600970609     C                   ENDSR
014700970609     c*--------------------------------------------------------------------------------------------*
014800970609     c* chkrec - controlla validità record
014900970609     c*--------------------------------------------------------------------------------------------*
015000970609     C     chkrec        BEGSR
015100970609     C*
015200970609     c                   MOVEL     'S'           $recok                         *record valido
015300970609     C*
015400970609     C* cap annullato, esclude
015500970609if  1c                   IF        cpcatb<>*blanks
015600970609     c                   MOVEL     'N'           $recok                         *record non valido
015700970609e   1c                   ENDIF
015800970609     C*
015900970609     C                   ENDSR
016000970609     c*--------------------------------------------------------------------------------------------*
016100970609     c* chkcap - controlla il cap
016200970609     c*--------------------------------------------------------------------------------------------*
016300970609     C     chkcap        BEGSR
016400970611     C*
016500970611     C* reimposta le variabili di lavoro
016600010924     C                   MOVEL     '0'           $err                           *no errore
016700010924     C*
016800010924     C* cap estero, esclude
016900010924if  1C                   IF        cpcnar<>*blanks
017000010924     C                   MOVEL     '1'           $err                           *errore
017100010924     C                   ELSE
017200010924     C*
017300010924     C* controlla se il campo è un numero
017400010924     C                   SETOFF                                       28
017500010924     c                   MOVEL     cpccap        wa5                            *cap di 5
017600010924     C                   IF        %subst(wa5:3:3) = *blanks
017700010924     C                   MOVE      '000'         wa5
017800010924     C                   ENDIF
017900010924     c                   MOVE      wa5           wa1                            *ultimo byte
018000010924     C                   TESTN                   wa5                  28
018100010924if  2c                   IF        *in28 AND                                    *è un numero
018200010924     c                             wa1>='0'
018300970609     C*
018400970609     C* lancia il programma che controlla il cap
018500970609     c                   CLEAR                   dssi95                         *azzera i parametri
018600970609     c                   MOVEL     *blanks       i95tla                         *tipo lancio
018700970609     c                   MOVEL     '3'           i95tcn                         *tipo controllo
018800970609     c                   MOVEL     cpcnar        i95nar                         *nazione
018900010924     c                   MOVEL     wa5           i95cap                         *cap
019000970703     c                   MOVEL     p50dri        i95dat                         *data di riferimento
019100970618     c                   MOVEL     p50fil        i95tfp                         *terminal partenza
019200980602     c                   MOVEL     'S'           i95fre                         *controlla eccezioni
019300970609     c                   CALL      'TISI95R'
019400970609     c                   PARM                    dssi95
019500970609     c*
019600970609     c* se trova l'eccezione, sostituisce ai campi del cap quelli dell'eccezione
019700970611if  1c                   IF        o95err=*blanks                               *no errori
019800970609if  2c                   IF        o95ela>*zeros                                *esiste eccezione
019900970609     c                   Z-ADD     o95ela        cpclna
020000970609     c                   Z-ADD     o95ezc        cpcznc
020100970609     c                   Z-ADD     o95elo        cpclol
020200970609     c                   Z-ADD     o95ezo        cpczol
020300970609     c                   Z-ADD     o95els        cpclos
020400970609     c                   Z-ADD     o95ezs        cpczos
020500971118     c                   Z-ADD     o95el2        cpclo2
020600971118     c                   Z-ADD     o95ez2        cpczo2
020700970609e   2c                   ENDIF
020800970609x   1c                   ELSE
020900010924if  2c                   IF        cpcfit<>'S'                                  *no cap fittizio
021000970609     c                   MOVEL     '1'           $err                           *errore
021100970611     c                   EXSR      staerr                                       *stampa errore
021200970611e   2c                   ENDIF
021300970611e   1c                   ENDIF
021400980310     c*
021500980310     c* calcola la linea arrivo da considerare
021600980310if  1c                   IF        o95err=*blanks                               *no errori
021700980310     c                   Z-ADD     *zeros        wlin                           *linea arrivo
021800980310     c                   Z-ADD     *zeros        wzon                           *zona consegna
021900980310     c                   EXSR      callna
022000980310e   1c                   ENDIF
022100010924     C*
022200010924     C                   ELSE
022300010924     C                   MOVEL     '1'           $err                           *errore
022400010924     C                   ENDIF
022500010924     C                   ENDIF
022600970609     C*
022700970609     C                   ENDSR
022800980310     c*--------------------------------------------------------------------------------------------*
022900980310     C* callna - calcola la linea arrivo da considerare
023000980310     c*--------------------------------------------------------------------------------------------*
023100980310     c     callna        BEGSR
023200980310     c*---
023300010703     c* BARTOLINI -> prende sempre la linea arrivo standard
023400010703     c* S.D.I.    -> considera la linea arrivo oltre se questa è una filiale SDI, altrimenti
023500010703     c*              prende la linea arrivo standard
023600980310     c*---
023700010703if  1c                   IF        p50bos = 'S'                                 *S.D.I.
023800010703if  2c                   IF        cpclol > *zeros                              *esiste lna oltre
023900010703     c                   Z-ADD     1             i
024000010703     c                   SETOFF                                           28
024100010703     c     cpclol        LOOKUP    org(i)                                 28
024200010703if  3c                   IF        *in28 AND                                    *trovata
024300020805     c                             odit(i) = 'SDI'                              *filiale sdi
024400010703     c                   Z-ADD     cpclol        wlin                            -linea oltre
024500010703     c                   Z-ADD     cpczol        wzon                            -zona oltre
024600010703x   3c                   ELSE                                                   *non trovata/non SDI
024700010703     c                   Z-ADD     cpclna        wlin                            -linea standard
024800010703     c                   Z-ADD     cpcznc        wzon                            -zona standard
024900010703e   3c                   ENDIF
025000010703x   2c                   ELSE                                                   *non esiste oltre
025100010703     c                   Z-ADD     cpclna        wlin                            -linea standard
025200010703     c                   Z-ADD     cpcznc        wzon                            -zona standard
025300010703e   2c                   ENDIF
025400010703x   1c                   ELSE                                                   *BARTOLINI
025500980310     c                   Z-ADD     cpclna        wlin                            -linea standard
025600980310     c                   Z-ADD     cpcznc        wzon                            -zona standard
025700010703e   1c                   ENDIF
025800980310     c*
025900980310     c                   ENDSR
026000970609     c*--------------------------------------------------------------------------------------------*
026100970609     C* scrist0 - scrive il cap sul file di output
026200970609     c*--------------------------------------------------------------------------------------------*
026300970609     C     scrist0       BEGSR
026400970609     C*
026500970609     C* imposta i campi del record da scrivere
026600000911     C                   MOVEL     wa5           a_istcap
026700980310     C                   Z-ADD     wlin          a_istlna
026800980310     C                   Z-ADD     wzon          a_istlas
026900970609     C                   MOVEL     cpcflg        a_istflg
027000970609     C                   MOVEL     cpcprv        a_istprv
027100970609     C                   MOVEL     cpcpra        a_istcc5
027200970609     C                   MOVEL     cpciso        a_istiso
027300970609     C                   MOVEL     cpcftr        a_istftr
027400970711     C                   Z-ADD     cpcdtr        a_istdtr
027500970609     C                   MOVEL     cpcatb        a_istatb
027600010918     C*
027700010918     C* Se richiesto azzeramento linne testo qui e procedo
027800010918     C                   IF        p50azz = 'S'
027900010921     C                   IF        a_istlna <> *zeros
028000010921     C                   EVAL      a_istlna = 789
028100010921     C                   ENDIF
028200010918     C                   EVAL      a_istlas = *zeros
028300010918     C                   ENDIF
028400010918     C*
028500970609     C* scrive il record
028600970609     C                   WRITE     azist0
028700970609     C*
028800970609     C                   ENDSR
028900970609     c*--------------------------------------------------------------------------------------------*
029000970609     C* scrist1 - scrive il cap sul file di output
029100970609     c*--------------------------------------------------------------------------------------------*
029200970609     C     scrist1       BEGSR
029300970609     C*
029400970609     C* imposta i campi del record da scrivere
029500000911     C                   MOVEL     wa5           b_istcap
029600980310     C                   MOVEL     wlin          b_istlna
029700980310     C                   MOVEL     wzon          b_istlas
029800970711     C                   MOVEL     cpcflg        b_istflg
029900970711     C                   MOVEL     cpcprv        b_istprv
030000970711     C                   MOVEL     cpcpra        b_istcc5
030100970711     C                   MOVEL     cpciso        b_istiso
030200970711     C                   MOVEL     cpcftr        b_istftr
030300980310     C                   MOVEL     *zeros        b_istdtr
030400980310     C                   MOVEL     'W'           b_istatb
030500010918     C*
030600010918     C* Se richiesto azzeramento linne testo qui e procedo
030700010918     C                   IF        p50azz = 'S'
030800010921     C                   IF        b_istlna <> *zeros
030900010921     C                   EVAL      b_istlna = '789'
031000010921     C                   ENDIF
031100010918     C                   EVAL      b_istlas = *zeros
031200010918     C                   ENDIF
031300010918     C*
031400970609     C* scrive il record
031500970609     C                   WRITE     azist1
031600970609     C*
031700970609     C                   ENDSR
031800970609     c*--------------------------------------------------------------------------------------------*
031900970609     C* scrcapx - scrive il cap sul file di output
032000970609     c*--------------------------------------------------------------------------------------------*
032100970609     C     scrcapx       BEGSR
032200970609     C*
032300970609     C* imposta i campi del record da scrivere
032400970609     C                   MOVEL     cpcnar        capnar
032500970609     C                   MOVEL     cpccap        capcap
032600970609     C                   Z-ADD     cpclna        caplna
032700970609     C                   Z-ADD     cpcznc        capznc
032800970609     C                   MOVEL     cpcprv        capprv
032900970609     C                   MOVEL     cpcpra        cappra
033000970609     C                   MOVEL     cpcflg        capflg
033100970609     C                   MOVEL     cpcfit        capfit
033200970609     C                   MOVEL     cpciso        capiso
033300970609     C                   Z-ADD     cpczfd        capzfd
033400970722     c                   EVAL(H)   wn115=(cpclkg/100)
033500970908if  1c                   IF        wn115 = *zeros OR
033600970908     c                             wn115 > 999
033700970722     c                   Z-ADD     999           caplql
033800970908x   1c                   ELSE
033900970908     c                   Z-ADD     wn115         caplql
034000970611e   1c                   ENDIF
034100970908if  1c                   IF        caplmc = *zeros OR
034200970908     c                             caplmc > 999
034300970722     c                   Z-ADD     999           caplmc
034400970908x   1c                   ELSE
034500970908     C                   Z-ADD     cpclmc        caplmc
034600970611e   1c                   ENDIF
034700970908     C                   Z-ADD     cpclol        caplol
034800970722if  1c                   IF        caplol=*zeros
034900970722     C                   Z-ADD     cpclna        caplol
035000970722e   1c                   ENDIF
035100970609     C                   Z-ADD     cpczol        capzol
035200970722if  1c                   IF        capzol=*zeros
035300970722     C                   Z-ADD     cpcznc        capzol
035400970722e   1c                   ENDIF
035500970609     C                   MOVEL     cpccts        capcts
035600010918     C*
035700010918     C* Se richiesto azzeramento linne testo qui e procedo
035800010918     C                   IF        p50azz = 'S'
035900010921     C                   IF        caplna <> *zeros
036000010921     C                   EVAL      caplna = 789
036100010921     C                   ENDIF
036200010918     C                   EVAL      capznc = *zeros
036300010918     C                   EVAL      capzfd = *zeros
036400010921     C                   IF        caplol <> *zeros
036500010921     C                   EVAL      caplol = 789
036600010921     C                   ENDIF
036700010921     C                   EVAL      capzol = *zeros
036800010918     C                   ENDIF
036900010918     C*
037000010918     C* Scrive il record
037100970609     C                   WRITE     azcapx
037200970609     C*
037300970609     C                   ENDSR
037400970609     c*--------------------------------------------------------------------------------------------*
037500970618     C* imppar - Imposta le variabili di lavoro in base ai parametri di entrata
037600970609     c*--------------------------------------------------------------------------------------------*
037700970618     C     imppar        BEGSR
037800970703     C*
037900970703     C* reperimento versione cappario
038000970703if  1c                   IF        p50dri=*blanks
038100970707     c                   Z-ADD     *zeros        p96dri
038200970703x   1c                   ELSE
038300970707     c                   MOVEL     p50dri        p96dri
038400970703e   1c                   ENDIF
038500970707     c                   Z-ADD     *zeros        p96ver
038600970707     c                   CALL      'TISI96R'
038700970707     c                   PARM                    p96dri
038800970707     c                   PARM                    p96ver
038900970707if  1c                   IF        p96ver=*zeros                                *not version found
039000970703     c                   MOVEL     '1'           $err                           *errore
039100970703e   1c                   ENDIF
039200970618     C*
039300970703     C* imposta i campi se non passati
039400970618if  1c                   IF        p50fil=*blanks                               parametri no passati
039500970618     c                   MOVEL     simfel        p50fil                         *filiale
039600970618e   1c                   ENDIF
039700970703if  1c                   IF        p50dri=*blanks                               parametri no passati
039800971020     c                   MOVEL     datcor        p50dri                         *data riferimento
039900970703e   1c                   ENDIF
040000970703     C*
040100970703     C                   ENDSR
040200970609     c*--------------------------------------------------------------------------------------------*
040300970609     C* staerr - stampa errore in richiamo programma controllo cap
040400970609     c*--------------------------------------------------------------------------------------------*
040500970609     C     staerr        BEGSR
040600970609     c*
040700970609     c* fine pagina, stampa la testata
040800970609if  1c                   IF        *inof
040900970609     c                   EXCEPT    tes
041000970609     c                   SETOFF                                           of
041100970609e   1c                   ENDIF
041200970609     c*
041300970609     c* stampa riga di errore
041400970609     c                   EXCEPT    det
041500970609     c*
041600970609     c                   ENDSR
041700970609     c*--------------------------------------------------------------------------------------------*
041800970609     C* finsr - operazioni finali
041900970609     c*--------------------------------------------------------------------------------------------*
042000970609     C     finsr         BEGSR
042100970609     c*
042200970609     c* lancia in chiusura file il programma di controllo cap
042300970609     c                   CLEAR                   dssi95
042400970609     c                   MOVEL     'C'           i95tla
042500970609     c                   CALL      'TISI95R'
042600970609     c                   PARM                    dssi95
042700970609     c
042800970609     c                   ENDSR
042900980310     c*--------------------------------------------------------------------------------------------*
043000980310     c* cartab - carica le tabelle occorrenti
043100980310     c*--------------------------------------------------------------------------------------------*
043200980310     c     cartab        BEGSR
043300980310     c*---
043400980310     c* filiali
043500980310     c*---
043600980310     c                   Z-ADD     *zeros        i
043700980310     c                   READ      azorg01l                               99
043800980310do  1c                   DOW       NOT *in99
043900980310if  2c                   IF        orgfva = *blanks AND
044000980310     c                             orgfag <> 'V'
044100980310     c                   ADD       1             i
044200980310     c                   Z-ADD     orgfil        org(i)                         *filiale
044300980310     c                   MOVEL     orgdit        odit(i)                        *ditta
044400980310e   2c                   ENDIF
044500980310     c                   READ      azorg01l                               99
044600980310e   1c                   ENDDO
044700980310     c*
044800980310     c                   ENDSR
044900020919     C*--------------------------------------------------------------------------------------------*
045000020919     C* REPERISCE I DATI UTENTE
045100020919     C*--------------------------------------------------------------------------------------------*
045200020919     C     REPDATIUTE    BEGSR
045300020919     C*
045400020919     C* INIZIALIZZA VARIABILI DI WRK
045500020919     C                   CLEAR                   TIBS34DS
045600020919     C                   CLEAR                   AZUTEDS
045700020919     C                   CLEAR                   DDATIUTE
045800020919     C*
045900020919     C     *DTAARA       DEFINE    §azute        azuteds
046000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
046100020919     C                   IN(E)     *DTAARA
046200020919if  1C                   IF        %Error
046300020919     c                   EVAL      I34Tla = 'L'
046400020919     C                   CALL      'TIBS34R'
046500020919     C                   PARM                    Tibs34Ds
046600020919     C                   IN        *DTAARA
046700020919e   1C                   ENDIF
046800020919     C*
046900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
047000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
047100020919     C*
047200020919     C                   ENDSR
047300970609     c*--------------------------------------------------------------------------------------------*
047400970609     C* *inzsr - operazioni iniziali
047500970609     c*--------------------------------------------------------------------------------------------*
047600970609     C     *inzsr        BEGSR
047700980310     c*---
047800970609     c* ricevimento parametri
047900980310     c*---
048000970609     c     *ENTRY        PLIST
048100970609     c                   PARM                    kpjba
048200970609     c                   MOVEL     kpjbu         dspar50
048300980310     C*---
048400970609     C* chiavi di lettura
048500980310     C*---
048600970618     C     keycpc        KLIST                                                  di azcpc01l
048700970618     C                   KFLD                    kccver                         -versione
048800980310     C*---
048900980310     C* variabili di controllo
049000980310     C*---
049100980310     c                   MOVEL     '1'           *inof
049200970609     C*
049300970609     C* reperimento dati utente
049400020919     C                   EXSR      REPDATIUTE
049500980310     C*
049600980310     C* reperimento s.informativo
049700020805     C                   CLEAR                   tibs55ds
049800980310     C                   MOVEL     'L'           i50tla
049900020805     C                   CALL      'TIBS55R'
050000020805     C                   PARM                    tibs55ds
050100970609     C*
050200970609     C* reperimento data corrente
050300971020     C                   TIME                    wn14                           *ora (6)+ data (8)
050400971020     C                   MOVE      wn14          wn8                            *data (8) in g/m/aa
050500971020     C                   Z-ADD     wn8           g08dat
050600971020     C                   Z-ADD     *zeros        g08inv
050700971020     C                   MOVEL     '0'           g08err
050800971020     c                   CALL      'XSRDA8'
050900971020     c                   PARM                    wlbda8
051000971020     C                   Z-ADD     g08inv        datcor                         *Data corrente
051100980310     C*
051200980310     C* carica le tabelle occorrenti
051300980310     C                   EXSR      cartab
051400970609     C*
051500970609     C                   ENDSR
051600970609     o*--------------------------------------------------------------------------------------------*
051700970609     o* Stampa
051800970609     o*--------------------------------------------------------------------------------------------*
051900970609     Oqsysprt   E            tes               2
052000970609     O                       slin(1)             66
052100970609     O                       slin(2)            132
052200970609     O                       dsfirs              20
052300971020     O                       datcor        8    102
052400970609     O                       page          z    132
052500970609     O          E            tes               3
052600970609     O                       slin(3)             66
052700970609     O                       slin(3)            132
052800970609     O          E            tes               4
052900970609     O                       slin(4)             66
053000970609     O          E            tes               5
053100970609     O                       slin(3)             66
053200970609     O                       slin(3)            132
053300970609     O*------------------------
053400970609     O          E            det         1
053500970611     o                       cpcnar               3
053600970611     o                       cpccap              13
053700970609     o                       o95err              16
053800970609     o                       o95msg              98
053900970609     o*--------------------------------------------------------------------------------------------*
054000970609     o* righe di stampa
054100970609     o*--------------------------------------------------------------------------------------------*
054200970609** slin - righe di stampa
054300970610xxxxxxxxxxxxxxxxxxxx        TISI52R     *** ALLINEAMENTO CAPPARIO 1
054400970610CLIENTI ***               99/99/9999                      PAG.99992
054500970609------------------------------------------------------------------3
054600970609___ C A P ___  ERRORE                                             4
054700970609xxx xxxxxxxxx  X XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX5
054800970609XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                   6
0549009706097
0550009706098
0551009706099
05520097060910
