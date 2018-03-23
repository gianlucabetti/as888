000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400060509      *  Legge responsabilità danni
000500051227      *-----------------------------------------------------------------*
000600051227     ftntud01l  uf a e           k disk
000700060509     ftabel00f  if   e           k disk
000800060509     fazorg01l  if   e           k disk
000900060511     ftntbe01l  if   e           k disk
001000051227
001100060511     d CCH             s              2    dim(50)
001200060511     d
001300051229     d conta           s                   like(tudqta)
001400051229     d dataalfa        s              6
001500060509     d codut           s              1  0 inz(1)
001600051227     d dtaiso          s               d   inz
001700060509     d Dataiso         s               d   datfmt(*iso)
001800060509     d Datafine        s               d   datfmt(*iso)
001900060509     d kmult           s                   like(tudmult)
002000060509     d savdata         s                   like(dcfdfc)
002100060509     d savpor          s                   like(dcrpor)
002200060509     d savaac          s                   like(dcraac)
002300060509     d savfil          s                   like(dcrfil)
002400060509     d savnca          s                   like(dcrnca)
002500060511     d kcod            s                   like(tblcod)
002600060511     d kkey            s                   like(tblkey)
002700060511     d kcod3           s                   like(tbecod) inz('CCH')
002800060511     d kke1            s                   like(tbeke1)
002900060104     d keytud          ds                  likerec(tntud000:*key)
003000051229     d wrkgetlista     s           4096    varying
003100051227
003200060511     d DCCH          e ds
003300060511     d DS05          e ds
003400060509     d dtup          e ds
003500060104     d fndcfds       e ds                  extname(fndcf00f)
003600060509     d fndcrds       e ds                  extname(fndcr00f)
003700060511     d fndcTds       e ds                  extname(fndcT00f)
003800051227     d kpjba         e ds
003900051227     d tntu05ds      e ds
004000060104     dtntudtads      e ds
004100051227
004200051227      *-----------------------------------------------------------------*
004300051227      * Moltiplicatori calcolati:
004400060509      *    505 - numero c.a. per p.o. responsabile
004500051227      *-----------------------------------------------------------------*
004600060509     c* Cancello se già trovo il numeratore
004700060509     c                   eval      kmult='505'
004800060509     c     *iso          movel     t05dti        dataiso
004900060509     c     *iso          movel     t05dtf        datafine
005000060509    2c                   dow       dataiso<=datafine
005100060509     c                   exsr      CANCMULT
005200060509     c
005300060509     c                   adddur    1:*D          dataiso
005400060509    2c                   enddo
005500051229
005600060511     c                   exsr      CARCCH
005700060511     c                   exsr      PREPASQL
005800060104     c
005900060104     c*  ?Aggiorno data ultima elaborazione in tabella TUP
006000060104     c                   clear                   tntudtads
006100060104     c                   eval      §DTAazn = kcdaz
006200060104     c                   eval      §DTAdta = t05dtf
006300060104     c                   call      'TNTUDTAR'
006400060104     c                   parm                    tntudtads
006500051229
006600051229     c                   Eval      *InLr = *On
006700051229
006800051229      *-----------------------------------------------------------------*
006900060104      * Lettura file fasi
007000051229      *-----------------------------------------------------------------*
007100060104     c     prepasql      begsr
007200051229
007300051229      *?Imposto la parte fissa dell'istruzione sql
007400051229     c                   eval      wrkgetlista =
007500060509     c                             'select dcrpor, dcfdfc, dcraac, dcrfil, -
007600060511     c                             dcrnca, dctdch, dctcch, dcttcr  -
007700060510     c                             from fndcr00f, fndcf00f, fndct00f where -
007800060509     c                             dcfdfc between '+
007900060509     c                             %editc(t05dti:'X') + ' and ' +
008000060510     c                             %editc(t05dtf:'X') + ' and dcffca=010' +
008100060511     c                              ' and dcrpor>0 and dcttcr<>''S'''
008200060509      *?Unione dei 2 files
008300060509     c                   eval      wrkgetlista = wrkgetlista +
008400060509     c                             ' and dcrfil=dcffil and dcraac=dcfaac ' +
008500060510     c                             ' and dcrnca=dcfnca and dcrfil=dctfil ' +
008600060510     c                             ' and dcrnca=dctnca and dcraac=dctaac '
008700060509      *?Ordinamento
008800060509     c                   eval      wrkgetlista = wrkgetlista +
008900060509     c                             ' order by dcrpor, dcfdfc, dcraac, dcrfil, '+
009000060509     c                             ' dcrnca'
009100051229      *?Solo lettura
009200051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
009300051229
009400051229     C/EXEC SQL
009500051229     C+ PREPARE s1 FROM :wrkgetlista
009600051229     C/END-EXEC
009700051229     C
009800051229     C/EXEC SQL
009900051229     C+ DECLARE A1 CURSOR FOR S1
010000051229     C/END-EXEC
010100051229     C
010200051229     C/EXEC SQL
010300051229     C+ OPEN a1
010400051229     C/END-EXEC
010500051229
010600051229     c                   clear                   conta
010700051229      *?Leggo il file
010800051229     c                   do        *hival
010900051229     C/EXEC SQL
011000060509     C+ FETCH NEXT FROM a1 INTO :dcrpor, :dcfdfc, :dcraac, :dcrfil,
011100060511     c+                         :dcrnca, :dctdch, :dctcch, :dcttcr
011200051229     C/END-EXEC
011300051229
011400051229     c                   select
011500051229
011600051229     c                   when      sqlcod = 100
011700060509     c* Ultima scrittura
011800060509     c                   if        conta>0 and savpor>0 and savdata>0
011900060509     c                   exsr      ScriviTNTU
012000060509     c                   endif
012100060509     c
012200051229     c                   leave
012300051229     c                   when      sqlcod < 0
012400051229     c                   seton                                        H1
012500051229     c                   exsr      srfine
012600051229     c                   other
012700060104     c
012800060509     c* Leggi C.A.
012900060509     c                   exsr      LeggiCA
013000060104     c
013100051229     c                   endsl
013200051229
013300051229     c                   enddo
013400051229
013500051229     C/EXEC SQL
013600051229     C+ CLOSE a1
013700051229     C/END-EXEC
013800051229
013900051229     c                   endsr
014000060509      *-----------------------------------------------------------------*
014100060509      * Leggo i record selezionati e scrivo u dato per area
014200060509      *-----------------------------------------------------------------*
014300060509     c     LeggiCA       begsr
014400060509     c* Se cambia p.o. memorizzo il dato
014500060509     c                   if        savpor<>dcrpor
014600060511     c                   if        savpor>0 and conta>0
014700060509     c                   exsr      ScriviTNTU
014800060509     c                   endif
014900060509     c                   clear                   conta
015000060509     c                   eval      savpor=dcrpor
015100060509     c                   eval      savdata=dcfdfc
015200060509     c                   clear                   savaac
015300060509     c                   clear                   savfil
015400060509     c                   clear                   savnca
015500060509     c                   endif
015600060509     c
015700060509     c* se cambia data memorizzo
015800060509     c                   if        savdata<>dcfdfc
015900060511     c                   if        savdata>0  and conta>0
016000060509     c                   exsr      ScriviTNTU
016100060509     c                   endif
016200060509     c                   clear                   conta
016300060509     c                   eval      savpor=dcrpor
016400060509     c                   eval      savdata=dcfdfc
016500060509     c                   clear                   savaac
016600060509     c                   clear                   savfil
016700060509     c                   clear                   savnca
016800060509     c                   endif
016900060509     c
017000060509     c* conto le c.a.
017100060511     c* Escludo le c.a. chiuse con causale che non prevede la responsab
017200060511     c                   setoff                                       35
017300060511     c                   if        dctcch<>'  '
017400060511     c     dctcch        lookup    cch                                    35
017500060511     c                   endif
017600060511     c
017700060511     c* Escludo le c.a. con evento per il quale non si calcola la
017800060511     c*  responsabilità
017900060511     c                   if        not *in35 and dcttcr<>'S'
018000060510     c                   if        savaac<>dcraac or  savfil<>dcrfil
018100060510     c                             or  savnca<>dcrnca
018200060509     c                   add       1             conta
018300060509     c                   eval      savaac=dcraac
018400060509     c                   eval      savfil=dcrfil
018500060509     c                   eval      savnca=dcrnca
018600060509     c                   endif
018700060511     c                   endif
018800060509     c
018900060509     c                   endsr
019000051227      *-----------------------------------------------------------------*
019100051228      * Scrivo/Aggiorno TNTUD
019200051227      *-----------------------------------------------------------------*
019300051227     c     scrivitntu    begsr
019400060509     c* Verifico su quale p.o. memorizzare i dati
019500060509     c                   eval      pocapo=savpor
019600060509     c
019700060509     c     savpor        chain     azorg01l
019800060509     c                   if        %found(azorg01l)
019900060509     c* con il dato AREA chaino tabella 05 per avere p.o. capofila
020000060509     c                   eval      kcod='05'
020100060509     c                   movel(P)  orgcar        kkey
020200060509     c     ktab          chain     tabel
020300060509     c                   if        %found(tabel00f)
020400060509     c                   movel     tbluni        ds05
020500060509     c                   if        §05pcp>0
020600060509     c                   movel     §05pcp        pocapo            3 0
020700060509     c                   endif
020800060509     c                   endif
020900060509     c                   endif
021000051229
021100051229      /free
021200051229
021300051229       //?imposto la data iso
021400060509       dataalfa = %subst(%char(savdata):3:6);
021500051229       dtaiso = %date(dataalfa:*ymd0);
021600051229
021700051229       //?imposto la chiave per tntud01l
021800060509       keytud.tudmult = kmult;
021900060510       keytud.tudfil = pocapo;
022000051229       keytud.tuddta = dtaiso;
022100051229
022200051229       //?aggancio tntud01l
022300051229       chain %kds(keytud:3) tntud01l;
022400051229
022500051229       //?trovo rcd aggiorno se è un ricalcolo
022600051229       if %found(tntud01l);
022700060509       tudqta = tudqta+conta;
022800051229       update tntud000;
022900051229
023000051229       //?non trovo rcd scrivo
023100051229       else;
023200051229       clear tntud000;
023300060509       tudmult = kmult;
023400051229       tuddta = dtaiso;
023500060510       tudfil = pocapo;
023600051229       tudcli = 0;
023700051229       tudqta = conta;
023800051229       write tntud000;
023900051229       endif;
024000051229
024100051229      /end-free
024200051227
024300051229     c                   endsr
024400051229
024500051229      *-----------------------------------------------------------------*
024600051229      * Fine programmma con codice errore
024700051229      *-----------------------------------------------------------------*
024800051229     c     srfine        begsr
024900051229
025000051229     c                   eval      *InLr = *On
025100051229     c                   return
025200051229
025300051229     c                   endsr
025400051227
025500060509     C**************************************************************************
025600060509     c* Cancella il moltiplicatore se rielaborazione
025700060509     C**************************************************************************
025800060509     C     CANCMULT      BEGSR
025900060509     c     ktud2         setll     tntud01l
026000060509     c
026100060509     c                   setoff                                       31
026200060509     c                   dow       not *in31
026300060509     c     ktud2         delete    tntud01l                           31
026400060509     c                   enddo
026500060509     c
026600060509     c                   ENDSR
026700060511      *-----------------------------------------------------------------*
026800060511      * Routine iniziale
026900060511      *-----------------------------------------------------------------*
027000060511     c     CARCCH        begsr
027100060511     c     kcod3         setll     tntbe01l
027200060511     c     kcod3         reade     tntbe01l
027300060511     c
027400060511     c                   z-add     1             x                 3 0
027500060511     c                   dow       not %eof(tntbe01l)
027600060511     c                   if        tbeatb=' '
027700060511     c                   movel     tbeuni        dcch
027800060511     c                   if        §cchnre='S'
027900060511     c                   movel     tbeke1        cch(x)
028000060511     c                   add       1             x
028100060511     c                   endif
028200060511     c                   endif
028300060511     c
028400060511     c     kcod3         reade     tntbe01l
028500060511     c                   enddo
028600060511     c
028700060511     c                   ENDSR
028800051227      *-----------------------------------------------------------------*
028900051227      * Routine iniziale
029000051227      *-----------------------------------------------------------------*
029100051227     c     *inzsr        begsr
029200051227
029300051227     c     *entry        plist
029400051227     c                   parm                    kpjba
029500051227     c                   eval      tntu05ds = kpjbu
029600060509     c
029700060509     c     ktud2         Klist
029800060509     c                   Kfld                    kmult
029900060509     c                   Kfld                    dataiso
030000060509     c     ktab          Klist
030100060509     c                   Kfld                    codut
030200060509     c                   Kfld                    kcod
030300060509     c                   Kfld                    kkey
030400051227
030500051229     c                   endsr
