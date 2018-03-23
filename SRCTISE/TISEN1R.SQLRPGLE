000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400091029     H* STATISTICA DI CONTROLLO NUMEROSITÀ LANCIO AZIONI
000500051227      *-----------------------------------------------------------------*
000600091029     fkplog00f  if a e             disk
000700091030     Fkfazn11l  IF   E           K DISK
000800091102     Fkpprf01l  IF   E           K DISK
000900091102     Fazorg01l  IF   E           K DISK
001000091102     fTISEN1P   o    E             printer oflind(*in80)
001100051227
001200091029     d datdti7         s              7  0
001300091029     d datdtf7         s              7  0
001400091030     d Wtipo           s              1
001500091030     d DECTIPO1        s             12
001600091030     d DECTIPO2        s             13
001700091030     d wfil            s              3
001800091030     d wute            s             10
001900091030     d savute          s             10
002000091102     d savFIL          s              3
002100091030     d wazn            s              4
002200091030     d savazn          s              4
002300091102     d WOK             s              1
002400091103     d Wtpaz           s              1
002500091030     d contaG          s              5  0
002600091030     d contaP          s                   like(ContaG)
002700091030     d Pconta          s                   like(ContaG)
002800091029     d dataiso         s               d   datfmt(*iso)
002900091030     d dataeur         s               d   datfmt(*eur)
003000091029
003100051229     d wrkgetlista     s           4096    varying
003200051227
003300051227     d kpjba         e ds
003400091029     D*---------------------
003500091029     D* DS PARAMETRI DI LANCIO
003600091029     D*------------------
003700091029     D TISEN0DS        DS                  INZ
003800091029     D  PARDTI                 1      8  0
003900091029     D  PARDTF                 9     16  0
004000091029     D  PARtipo               17     17
004100091030     D  Pazn                  18     97    dim(20)
004200091030     D  PARfrg                98    157    dim(20)
004300091030     D  PARfrp               158    217    dim(20)
004400091029     d
004500091029     D TIBS34DS      E DS                                                       *Profili utente
004600091029     D DDATIUTE      E DS                                                       *Dati utente
004700091029     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004800140930
004900140930      // - Parametri per gestione parametri batch
005000140930     d TRUL01ds      e ds                  inz
005100140930     d   UL01err     e                     inz(*off)
005200051227
005300091102     D TRMK13DS      E DS
005400091102     d
005500091102     d
005600091102     D PARta08         DS
005700091102     D* CODICE FILIALE
005800091102     D  t08FIL                 1      3  0
005900091102     D* SCELTA ORDINAMENTO STAMPA: 'C'=CODICE  'A'=RAGIONE SOCIALE
006000091102     D  T08SC1                 4      4
006100091102     D* RICHIESTA CLIENTI NON ABILITATI ALLA SPEDIZIONE
006200091102     D  T08SC2                 5      5
006300091102     D* RICHIESTA VOLUME AUTOMATICO
006400091102     D  T08SC3                 6      6
006500091102     D* RICHIESTA CODICE IMPORTANZA CLIENTE
006600091102     D  T08CIC                 7      7
006700091102      * tipo utilizzo peso VDL
006800091102     d  T08tap                 8      8
006900091102     D* NETWORK TARIFFE
007000091102     D  T08NTR                 9      9
007100091102     D* RICHIESTA TARIFFE CON ASSICURAZIONE PER CONTO
007200091102     D  T08SAC                10     10
007300091102     D* RICHIESTA ISTAT
007400091102     D  T08IST                11     11
007500091102      * flag rinuncia ac base
007600091102     d  T08racb               12     12
007700091102      * tariffa part. AC BASE
007800091102     d  T08base               13     13
007900091102     D* RICHIESTA TARIFFE bloccate
008000091102     D  T08blk                14     14
008100110923      * tariffa particolare
008200110923     d  T08ctp                15     15
008300091102     D* PARZIALIZZAZIONE PER CATEGORIA SEAT
008400091102     D  T08SNC                22     22
008500091102     D* CODICE CATEGORIA SEAT
008600091102     D  T08SCT                23     27  0
008700091103     D* non più usate!!
008800091103     D**T08SNM                28     28
008900091103     D**T08SCM                29     32
009000091102     D* RICHIESTA TIPO SERVIZIO
009100091102     D  T08TSP                33     33
009200091102     D* RICHIESTA TIPO TARIFFA
009300091102     D  T08TTR                94     94
009400091102     D* RICHIESTA CODICE PARTICOLARITA'
009500091102     D  T08CPA                95     96
009600091102     D* RICHIESTA TARIFFE CON CONSEGNA DDT
009700091102     D  T08BAM                97     97
009800091102     D* RICHIESTA TIPO STAMPA
009900091102     D  T08TPS                98     98
010000091102     D* Stampa Note Clienti
010100091102     D  t08TPN                99    100
010200091102     D* richiesta  Note Clienti
010300091102     D  t08nOTE              101    101
010400091102     D  t08TNOT              102    103
010500091102     d* Network
010600091102     D  T08NTW               104    106
010700091102     D* Codice agente
010800091102     D  T08AGE               107    113
010900091102     D  T08CNO               114    114
011000091102     D* Intervallo mese/anno per richiesta clienti non operativi
011100091102     D  T08DMM               115    116  0
011200091102     D  T08DAA               117    118  0
011300091102     D  T08AMM               119    120  0
011400091102     D  T08AAA               121    122  0
011500091102     d  T08bva               123    123
011600091102      * dati necessari x il caricamento della sk fig
011700091102     d  t08abi               124    125
011800091102     d  t08cdi               126    126
011900091102     d  t08car               127    129  0
012000091102     d  t08cpo               130    132  0
012100091102     D* parzializza tipo pagamento C/assegni
012200091102     D  T08tpc               133    133
012300091102     d* Luogo
012400091102     D  t08luog              134    134
012500091102     D  t08Tluo              135    137
012600121227      * Tassazione POST fatturazione
012700121227     d  T08tpf               138    138
012800091102     D*
012900140930      //---------------------------------------------------------------
013000140930      //?Definizione procedure usate.
013100140930      //---------------------------------------------------------------
013200140930
013300140930      // - Gestione parametri di lancio
013400140930     d TRUL01R         pr                  extpgm('TRUL01R')
013500140930     d  trul01ds                           likeds(TRUL01DS)
013600140930
013700051227      *-----------------------------------------------------------------*
013800091029     c                   eval      dataiso=%date(pardti:*iso)
013900091029     c                   eval      datdti7=%dec(dataiso:*cymd)
014000091029     c
014100091029     c                   eval      dataiso=%date(pardtf:*iso)
014200091029     c                   eval      datdtf7=%dec(dataiso:*cymd)
014300051229
014400060104     c                   exsr      PREPASQL
014500060104     c
014600091030     c* se non ha stampato nulla scrivo: nessuna anomalia riscontrata
014700091030     c                   if        dectipo1=*blanks
014800091102     c                   write     Sen1pnulla
014900091030     c                   else
015000091102     c                   write     Sen1pfine
015100091030     c                   endif
015200091030     c
015300051229     c                   Eval      *InLr = *On
015400051229
015500051229      *-----------------------------------------------------------------*
015600060104      * Lettura file fasi
015700051229      *-----------------------------------------------------------------*
015800060104     c     prepasql      begsr
015900051229
016000051229      *?Imposto la parte fissa dell'istruzione sql
016100051229     c                   eval      wrkgetlista =
016200091030     c                             'select  substr(logkpj, 4, 3),  -
016300091030     c                                      substr(logkpj, 1, 10),  -
016400091030     c                              substr(logkpj, 60, 4), -
016500091102     c                              logdai, logori, logdaf, logorf, -
016600091102     c                              logkpj  from kplog00f where -
016700091030     c                               logdai between '+
016800091030     c                               %editc(datdti7:'X') + ' and ' +
016900091030     c                               %editc(datdtf7:'X') + ' and   +
017000091030     c                               substr(logkpj, 60, 4) in('
017100091030      * ciclo sui codici azioni
017200091030     c                   Do        20            XX                4 0
017300091030     c                   If        pazn(XX) = *blanks
017400091030     c                   Leave
017500091030     c                   EndIf
017600091030     c
017700091030     c                   If        XX >1
017800091030     c                   eval      wrkgetlista =
017900091030     c                             %trim(wrkgetlista) + ','
018000091030     c                   EndIf
018100091030     c
018200091030     c                   eval      wrkgetlista =
018300091030     c                             %trim(wrkgetlista) + ' ''' +
018400091030     c                             pazn(xx) + ''''
018500091030     c                   EndDo
018600091030     c
018700091030     c                   Eval      wrkgetlista =
018800091030     c                             %trim(wrkgetlista) + ')'
018900051229      *?Ordinamento
019000051229     c                   eval      wrkgetlista = wrkgetlista +
019100091030     c                             ' order by substr(logkpj, 4, 3), +
019200091030     c                             substr(logkpj, 1, 10), +
019300091030     c                             substr(logkpj, 60, 4)'
019400051229      *?Solo lettura
019500051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
019600051229
019700051229     C/EXEC SQL
019800051229     C+ PREPARE s1 FROM :wrkgetlista
019900051229     C/END-EXEC
020000051229     C
020100051229     C/EXEC SQL
020200051229     C+ DECLARE A1 CURSOR FOR S1
020300051229     C/END-EXEC
020400051229     C
020500051229     C/EXEC SQL
020600051229     C+ OPEN a1
020700091030     c/END-EXEC
020800051229
020900051229      *?Leggo il file
021000051229     c                   do        *hival
021100051229     C/EXEC SQL
021200091030     C+ FETCH NEXT FROM a1 INTO :wfil, :wute, :wazn, :logdai, :logori, :logdaf,
021300091102     C+                         :logorf, :logkpj
021400051229     C/END-EXEC
021500051229
021600051229     c                   select
021700051229
021800051229     c                   when      sqlcod = 100
021900051229     c                   leave
022000051229     c                   when      sqlcod < 0
022100051229     c                   seton                                        H1
022200051229     c                   exsr      srfine
022300051229     c                   other
022400060104     c
022500091030     c                   exsr      Elabora
022600060104     c
022700051229     c                   endsl
022800051229
022900051229     c                   enddo
023000091030     c
023100091030     c* Eseguo ultimo controllo
023200091030     c                   clear                   wute
023300091030     c                   clear                   wazn
023400091030     c                   exsr      Verifica
023500091030     c
023600051229
023700051229     C/EXEC SQL
023800051229     C+ CLOSE a1
023900051229     C/END-EXEC
024000051229
024100051229     c                   endsr
024200051229
024300051229      *-----------------------------------------------------------------*
024400051229      * Fine programmma con codice errore
024500051229      *-----------------------------------------------------------------*
024600051229     c     srfine        begsr
024700051229
024800051229     c                   eval      *InLr = *On
024900051229     c                   return
025000051229
025100051229     c                   endsr
025200091030      *-----------------------------------------------------------------*
025300091030      * Elabora azione per azione per ogni utente
025400091030      *-----------------------------------------------------------------*
025500091030     c     Elabora       begsr
025600091102     c                   clear                   Wok
025700091102     C                   SELECT
025800091102     c                   when      wazn='MK13'
025900091102     c                   exsr      CtrMK13
026000091102     c
026100091102     c                   when      wazn='TA08'
026200091102     c                   exsr      CtrTA08
026300091102     c                   other
026400091102     c                   eval      wok='S'
026500091102     c                   ENDSL
026600091030     c
026700091102     c                   if        wok='S'
026800091102     c
026900091030     c* A  cambio utente, azione --> verifico la frequenza
027000091030     c                   if        savute=*blanks or savute<>wute
027100091030     c                   if        savute<>*blanks
027200091030     c                   exsr      verifica
027300091030     c                   endif
027400091030     c                   eval      savute=wute
027500091030     c                   eval      savazn=wazn
027600091102     c                   eval      savFIL=wFIL
027700091030     c                   clear                   contaG
027800091030     c                   clear                   contaP
027900091030     c                   endif
028000091030     c
028100091030     c                   if        savazn=*blanks or savazn<>wazn
028200091030     c                   if        savazn<>*blanks
028300091030     c                   exsr      verifica
028400091030     c                   endif
028500091030     c                   eval      savute=wute
028600091030     c                   eval      savazn=wazn
028700091102     c                   eval      savFIL=wFIL
028800091030     c                   clear                   contaG
028900091030     c                   clear                   contaP
029000091030     c                   endif
029100091030     c
029200091030     c* coteggio nel periodo
029300091030     c                   eval      contaP=contaP+1
029400091030     c* conteggio giornaliero
029500091030     c                   if        logdai=datdtf7
029600091030     c                   eval      contaG=contaG+1
029700091030     c                   endif
029800091102     c
029900091102     c                   endif
030000091030     c
030100091030     c                   endsr
030200091102      *-----------------------------------------------------------------*
030300091102      * controlla kpjbu azione MK13
030400091102      *-----------------------------------------------------------------*
030500091102     c     CtrMK13       begsr
030600140930
030700140930      /free
030800140930       //?da WFPRM00F
030900140930         clear TRMK13DS;
031000140930         reset TRUL01DS;
031100140930         UL01prg = %dec(%subst(logkpj:247:7):7:0);
031200140930         trul01r (trul01ds);
031300140930         IF  UL01err = *off;
031400140930           TRMK13DS = UL01prm;
031500140930         ENDIF;
031600140930      /end-free
031700140930
031800140930     c***!!!!!!          eval      trmk13ds=%subst(logkpj:247:256)
031900091103     c                   eval      wtpaz   =%subst(logkpj:64:1)
032000091103     c
032100091103     c*     Considero solo se lancio Batch (perescludere i tentativi interattivi
032200091103     c*      di Nicola!!)
032300091103     c                   if        wtpaz='B'
032400091102     c
032500091103     c* non considero se parzializzato troppo
032600091102     c                   if        d13cap=*blanks  and
032700091102     c                             d13loc=*blanks  and
032800110923     c                             d13cmmu=0       and
032900091102     c                             d13sct=0        and
033000110923     c                             d13cic=*blanks  and
033100091102     c                             d13cmm=0        and
033200110923     c                             d13cco=*blanks  and
033300110923     c                             d13dadda=0      and
033400110923     c                             d13dada=0       and
033500091102     c                             d13ifoal=0      and
033600091102     c                             d13nifo=' '     and
033700091102     c                             d13rip=*blanks  and
033800091102     c                             d13ripa=0       and
033900091102     c                             d13pri=' '      and
034000091102     c                             d13gdo=' '      and
034100091102     c                             d13ecm=' '      and
034200100608     c                             d13hpr=' '      and
034300110923     c                             d13sbx=' '      and
034400091102     c                             d13pesa=0       and
034500140930     c****                         d13pspa=0       and
034600091102     c                             d13conc=*blanks and
034700110923     c                             d13moi =*blanks and
034800091102     c                             d13logi=*blanks and
034900091102     c                             d13logo=*blanks and
035000110923     c                             d13logp=*blanks
035100091102     c                   eval      wok='S'
035200091102     c                   endif
035300091103     c                   endif
035400091102     c
035500091102     c                   endsr
035600091102      *-----------------------------------------------------------------*
035700091102      * controlla kpjbu azione TA08
035800091102      *-----------------------------------------------------------------*
035900091102     c     CtrTA08       begsr
036000091102     c                   eval      parta08 =%subst(logkpj:247:256)
036100091102     c                   if        T08sc3='N'      and
036200091102     c                             t08cic=' '      and
036300091102     c                             t08tap=' '      and
036400091102     c                             t08tap=' '      and
036500091102     c                             t08ntr='N'      and
036600091102     c                             t08sac='N'      and
036700091102     c                             t08ist='N'      and
036800091102     c                             t08racb='N'      and
036900091102     c                             t08base='N'      and
037000091102     c                             t08blk ='N'      and
037100110923     c                             t08ctp =' '      and
037200091102     c                             t08tsp =' '      and
037300091102     c                             t08ttr =' '      and
037400091102     c                             t08cpa ='  '      and
037500091102     c                             t08bam =' '      and
037600091102     c                             t08note='N'      and
037700091102     c                             t08age<= *zeros  and
037800091102     c                             t08sc2 ='N'      and
037900091102     c                             t08cno ='S'      and
038000091102     c                             t08tpc ='N'      and
038100121227     c                             t08luog='N'      and
038200121227     c                             T08tpf = 'N'
038300091102     c                   eval      wok='S'
038400091102     c                   endif
038500091102     c
038600091102     c                   endsr
038700091030      *-----------------------------------------------------------------*
038800091030      * Verifico se utente/azione da stampare in base alla frequenza
038900091030      *-----------------------------------------------------------------*
039000091030     c     Verifica      begsr
039100091030     c                   z-add     1             xx
039200091030     c     savazn        lookup    pazn(xx)                               30
039300091030     c
039400091030     c                   if        *in30
039500091030     c                   movel     parfrg(xx)    wfreqg            3 0
039600091030     c                   movel     parfrp(xx)    wfreqp            3 0
039700091030     c
039800091030     c                   if        contag>wfreqg  or
039900091030     c                             (contap>wfreqp  and partipo='R')
040000091030     c                   exsr      Stampa
040100091030     c                   endif
040200091030     c
040300091030     c                   endif
040400091030     c                   endsr
040500091030      *-----------------------------------------------------------------*
040600091030      * Stampo l'utente che ha utilizzato più del dovuto l'azione
040700091030      *-----------------------------------------------------------------*
040800091030     c     Stampa        begsr
040900091030     c* Stampa testata
041000091102     c                   if        *in80
041100091102     c                   write     sen1pt1
041200091102     c                   write     sen1pt3
041300091102     c                   setoff                                       80
041400091030     c
041500091030     C                   EVAL      DECTIPO1='NEL GIORNO :'
041600091030     C                   EVAL      DECTIPO2='NEL PERIODO :'
041700091030     c                   endif
041800091030     c
041900091102     C* SE il campo filiale è numerico decodifico, altrimenti
042000091102     c*  impsoto 046
042100091102     c                   clear                   p1cfil
042200091102     c* se profilo utente lungo  6 utente di sede
042300091102     c                   select
042400091102     c                   when      %subst(savute:7:1)=' '
042500091102     c                   eval      p1cfil=046
042600091102     c
042700091102     c                   when      savfil>*zeros
042800091102     c                   movel     savfil        p1cfil
042900091102     c                   other
043000091102     c                   eval      p1cfil=046
043100091102     c                   endsl
043200091102     c
043300091102     c     p1cfil        chain     azorg01l
043400091102     c                   if        %found(azorg01l)
043500091102     c                   movel     orgdes        p1dfil
043600091102     c                   else
043700091102     c                   eval      p1dfil=*all'?'
043800091102     c                   endif
043900091102     c
044000091102     c* Decodifico utente
044100091102     c     savute        chain     kpprf01l
044200091102     c                   if        %found(kpprf01l)
044300091102     c                   movel     prftxt        p1dute
044400091102     c                   else
044500091102     c                   eval      p1dute=*all'?'
044600091102     c                   endif
044700091102     c
044800091102     c                   write     Sen1pd1
044900091030     c
045000091030     c                   endsr
045100051227
045200051227      *-----------------------------------------------------------------*
045300051227      * Routine iniziale
045400051227      *-----------------------------------------------------------------*
045500051227     c     *inzsr        begsr
045600051227
045700091029     C*--------------------
045800091029     C* RICEVIMENTO PARAMETRI
045900091029     C*--------------------
046000091029     C     *ENTRY        PLIST
046100091029     C                   PARM                    KPJBA
046200091029     c                   movel     kpjbu         TISEN0DS
046300091029
046400091029     C                   CLEAR                   TIBS34DS
046500091029     C                   CLEAR                   AZUTEDS
046600091029     C                   CLEAR                   DDATIUTE
046700091029     C*
046800091029     C     *DTAARA       DEFINE    §azute        azuteds
046900091029     C     *DTAARA       DEFINE    §datiute      ddatiute
047000091029     C                   IN(E)     *DTAARA
047100091029if  1C                   IF        %Error
047200091029     c                   EVAL      I34Tla = 'L'
047300091029     C                   CALL      'TIBS34R'
047400091029     C                   PARM                    Tibs34Ds
047500091029     C                   IN        *DTAARA
047600091029e   1C                   ENDIF
047700091029
047800091029     C*
047900091030     c* Dati richiesti
048000091030     c                   clear                   dectipo1
048100091030     c                   clear                   dectipo2
048200091030     c
048300091030     c     *iso          movel     pardti        dataeur
048400091030     c                   movel     dataeur       gmadal            8 0
048500091030     c
048600091030     c     *iso          movel     pardtf        dataeur
048700091030     c                   movel     dataeur       gmaal             8 0
048800091030     c
048900091030     c* Stampa pagina selezioni
049000091102     c                   write     sen1pt1
049100091030     c
049200091030     c* Decodifico azioni scelte
049300091030     C                   SETON                                        02
049400091030     c                   z-add     1             xx
049500091030     c                   dow       xx<=20 and pazn(xx)<>*blanks
049600091030     c     pazn(xx)      chain     kfazn11l
049700091030     c                   if        %found(kfazn11l)
049800091030     c                   movel     kdsaz         p1dazn
049900091030     c                   else
050000091030     c                   eval      p1dazn='?'
050100091030     c                   endif
050200091030     c                   eval      p1cazn=pazn(xx)
050300091030     c
050400091030     c* frequenza giornaliera
050500091030     c                   movel     parfrg(xx)    p1cfrg            3
050600091030     c
050700091030     c                   if        partipo='R'
050800091030     c                   seton                                        01
050900091030     c* frequenza del periodo
051000091030     c                   movel     parfrp(xx)    p1cfrp            3
051100091030     c                   endif
051200091030     c
051300091102     c                   write     Sen1pt2
051400091030     C                   SETOFF                                       02
051500091030     c
051600091030     C                   ADD       1             XX
051700091030     C                   enddo
051800091030     c
051900091102     c                   seton                                        80
052000091029     C                   ENDSR
