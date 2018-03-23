000100000406      *------------------------------------------------------------------------*
000200000406      * Pulizia Eventi di messa in consegna
000300000406      *------------------------------------------------------------------------*
000400000406     H DECEDIT('0,') DATEDIT(*DMY.)
000500000406      *------------------------------------------------------------------------*
000600000406      * D A T A      B A S E
000700000406      *------------------------------------------------------------------------*
000800000411     FFNEVB03L  UF   E           K DISK
000900000406     F*-------
001000000406     FTITAS30C  IF   E           K DISK
001100000406     F*-------
001200000406     FTABEL00F  IF   E           K DISK
001300000406     F*-------
001400011023     FAZORG01L  IF   E           K DISK
001500011023     F*-------
001600040820     Ffnblp01L  IF   E           K DISK    usropn
001700040820     Ffnarb01L  IF   E           K DISK    usropn
001800040820     F*-------
001900000407     FQSYSPRT   O    F  132        PRINTER oflind(*inof)
002000020701      *------------------------------------------------------------------------*
002100000406      * D s
002200000406      *------------------------------------------------------------------------*
002300000406     D* ds architettura e passaggio parametri
002400000406     D KPJBA         E DS
002500000406     D* ds reperimento gg pulizia eventi messa in consegna
002600000406     D DS$3          E DS
002700011023     D* ds reperimento filale DPD
002800011023     D OG143         E DS
002900000406     D* ds x calcolo data pulizia (conversione data odierna in gg)
003000000406     D WLBDAT          DS
003100000406     D  G02DAT                 1      8  0
003200000406     D  G02INV                 9     16  0
003300000406     D  G02ERR                17     17
003400000406     D  G02TGI                18     22  0
003500000406     D* ds x calcolo data pulizia (conversione(gg data odierna - gg pul) in data puliz.)
003600000406     D WLBGIO          DS
003700000406     D  Gi8DAT                 1      8  0
003800000406     D  Gi8INV                 9     16  0
003900000406     D  Gi8TGI                17     21  0
004000040820     d comman          s             80
004100040820     d lenght          s             15  5 inz(80)
004200040820     d wlib            s             10    inz('FILTRA201 ')
004300040910     d wlibp           s             10    inz('FILTRAPRD ')
004400040820
004500040820     d cmd             s             60    dim(01) ctdata perrcd(1)
004600020701      *------------------------------------------------------------------------*
004700020701     ITITAS000      50
004800020701     ITITAS010      51
004900020701     ITITASP00      52
005000000406      *------------------------------------------------------------------------*
005100000406      * Flusso principale
005200000406      *------------------------------------------------------------------------*
005300000406      * Reperisco i gg. d pulizia eventi MIC da tabella e calcolo data
005400000406     C                   Exsr      OPEINI
005500000406      * Prima lettura (Reperisco codice filiale)
005600000406     C                   Exsr      NEWFLE
005700000406      * Loop lettura eventi MIC e pulizia
005800000406     C                   Dow       not *in30
005900000406      * Eseguo posizionamento all'interno della filiale x evento MIC
006000000406     C                   Exsr      CHKMIC
006100000406      * Loop x codice filiale, evento MIC e data evento
006200000406     C                   Dow       not *in30 and evbdev<=datpul and
006300000406     C                             evbfle=wsvfle and evbcev='MIC'
006400000406     C                   Exsr      ELABOR
006500000406     C                   End
006600000411     C   Of              except    Testa
006700000411     C                   except    totfle
006800000406      * Eseguo posizionamento su fle successiva
006900000406      * (vista logica evb x fle, cev, dta eve)
007000000406     C                   Exsr      NEWFLE
007100000406     C                   End
007200020704     C   Of              except    Testa
007300020704     C                   except    finale
007400000406      * Fine elaborazione
007500000406     C     Fine          TAG
007600000406     C                   SETON                                        LR
007700000406      *------------------------------------------------------------------------*
007800000406      * Operazioni iniziali: Calcolo la data di pulizia
007900000406      *------------------------------------------------------------------------*
008000000406     C     Opeini        Begsr
008100000406      *
008200000406      * Reperisco la data del giorno e ....
008300000406     C                   TIME                    Whhdat           14 0
008400000406      * .. la giro nel formato aaaa-mm-gg reperendo i gg.
008500000406     C                   Clear                   Wlbdat
008600000406     C                   MOVE      whhdat        g02dat
008700000406     C                   call      'XSRDA8'
008800000406     C                   parm                    Wlbdat
008900000406     C                   move      G02inv        oggi
009000000406      * Reperisco i giorni di pulizia degli eventi MIC
009100000406     C                   Clear                   DS$3
009200000406     C                   Z-add     1             kkut
009300000406     C                   Movel     '$3'          kcod
009400000406     C                   Movel     *blanks       kkey
009500000406     C                   Movel     '1'           kkey
009600000406     C     Ktab          Chain     Tabel00F                           31
009700000406     C                   if        not *in31 and tblflg=' '
009800000406     C                   Movel     tbluni        Ds$3
009900000406      * ... se non trovata tabella vaado a fine
010000000406     C                   Else
010100000406     C                   Goto      Fine
010200000406     C                   End
010300000406      * sottraggo i gg di pulizia da quelli della data del giorno
010400000406     C     G02Tgi        sub       §$3GEM        GGPUL             5 0
010500000406      * dai giorni di pulizia risalgo alla data
010600000406     C                   Clear                   Wlbgio
010700000406     C                   Z-ADD     ggpul         Gi8tgi
010800000406     C                   Call      'XSRGI8'
010900000406     C                   parm                    Wlbgio
011000000406     C                   movel     gi8inv        datpul
011100000406      *
011200000406      * Imposto filiale immissione evento a 0
011300000406     C                   Z-add     0             kfle
011400000406      * ... e codice evento a 'MIC'
011500000406     C                   Movel     'MIC'         kCev
011600000407      *
011700000407     C                   Seton                                        of
011800020704     C* contatori genrali
011900020704     C                   z-add     0             totgen            7 0
012000020704     C                   z-add     0             tot52p            7 0
012100000406     C*
012200000406     C                   Endsr
012300000406      *------------------------------------------------------------------------*
012400000406      * Newfle: Mi posiziono sulla prossima filiale immissione evento
012500000406      *------------------------------------------------------------------------*
012600000406     C     Newfle        Begsr
012700000406      *
012800011023     C     Oltre         tag
012900011023      *
013000000406     C     Kfle          setgt     fnevb03l
013100000407     C                   read      fnevb03l                               30
013200000406     C                   movel     evbfle        Kfle
013300011023      *
013400011023      * Non deve leggere gli eventi della filiale Dpd
013500011023      *    poichè non li deve cancellare
013600011023      *        test sulla filiale in organigramma per saltare le DPD.
013700011023     C     Kfle          chain     AzOrg01l
013800011023     C                   if        %Found(AzOrg01l)
013900011023     C                   eval      Og143 = OrgDE3
014000020717     C****** sostituito con il campo network
014100020717     C******             If        §OGDPD ='S'
014200020717     C                   If        §OgNtw ='DPD'
014300011023     C                   goto      Oltre
014400011023     C                   end
014500011023     C                   end
014600011023      *
014700000413     C                   Z-add     0             totmic            7 0
014800020704     C                   Z-add     0             tottas            7 0
014900011023      *
015000000406     C                   endsr
015100000406      *------------------------------------------------------------------------*
015200000406      * Chkmic: Se non l'ho già agganciato cerco prossimo evento MIC
015300000406      *------------------------------------------------------------------------*
015400000406     C     Chkmic        Begsr
015500000406      *
015600000406      * Se non ho già agganciato l'evento Mic mi posiziono sul Mic all'interno
015700000406      * della filiale di immissione dell'evento
015800000406     C                   if        evbcev<'MIC'
015900000406     C     Kevb          setll     fnevb03l
016000000406     C     kfle          reade     fnevb03l                               30
016100000406     C                   end
016200000406      * Se non sono a fine file eseguo il salvataggio dei dati
016300000406     C                   move      evbfle        wsvfle
016400000406      *
016500000406     C                   endsr
016600000406      *------------------------------------------------------------------------*
016700000406      * Elabor: Controllo se Mic è da cancellare
016800000406      *------------------------------------------------------------------------*
016900000406     C     Elabor        Begsr
017000000406      *
017100000406      * Da evento Mic aggancio la bolla
017200000406     C                   Z-add     Evbaas        Kaas
017300000406     C                   Z-add     EvbLnp        Klnp
017400000406     C                   Z-add     Evbnrs        Knrs
017500000406     C                   Z-add     Evbnsp        Knsp
017600020704      * spegne gli indicatori di tipo record del file combinato
017700020704     C                   setoff                                       505152
017800000406     C     Ktas          Chain     Titas30C                           32
017801061018
017802061018      * se la data dell'evento di messa in consegna è uguale alla data
017803061018      * di consegna cancello l'evento   (se poi non esistono ARB e BLP)
017804061018     C                   if        not *in32 and tasdcm=evbdev
017900040820
018000040820      * verifico anche non esistano ARB e BLP altrimenti non cancello
018100040820     C     Ktas          Chain     fnblp01l                           33
018200040820     C   33Ktas          Chain     fnarb01l                           33
018201061018
018300040820     c                   if        *in33
019700000411     C                   delete    fnevb000
019800000411     C                   add       1             totmic
019900020704     C                   add       1             totgen
021200000406     C                   end
021201061018      *
021300040820     C                   endIF
021301061018      *
021400000406      * leggo l'evento successivo all'interno della stessa filiale elaboratore
021500000406     C     Kfle          reade     fnevb03l                               30
021600000406      *
021700000406     C                   endsr
021800000406      *------------------------------------------------------------------------*
021900000406      * *inzsr: Definizione parametri e klist
022000000406      *------------------------------------------------------------------------*
022100000406     C     *inzsr        Begsr
022200000406      *
022300000406     C     *entry        plist
022400000406     C                   parm                    kpjba
022500000406      * Definizione klist
022600000406     C     ktab          klist
022700000406     C                   kfld                    Kkut
022800000406     C                   kfld                    Kcod
022900000406     C                   kfld                    Kkey
023000000406     C     kEvb          klist
023100000406     C                   kfld                    Kfle
023200000406     C                   kfld                    Kcev
023300000406     C     kTas          klist
023400000406     C                   kfld                    Kaas
023500000406     C                   kfld                    Klnp
023600000406     C                   kfld                    Knrs
023700000406     C                   kfld                    Knsp
023800000406      * Definizione campi
023900000406     C     *like         define    Tblkut        Kkut
024000000406     C     *like         define    Tblcod        Kcod
024100000406     C     *like         define    Tblkey        Kkey
024200000406     C     *like         define    evbfle        Kfle
024300000406     C     *like         define    evbcev        Kcev
024400000406     C     *like         define    tasaas        Kaas
024500000406     C     *like         define    taslnp        Klnp
024600000406     C     *like         define    tasnrs        Knrs
024700000406     C     *like         define    tasnsp        Knsp
024800000406     C     *like         define    evbdev        datpul
024900000406     C     *like         define    evbdev        oggi
025000000406     C     *like         define    evbfle        wsvfle
025100040820      * Apro il Fnblp di filiale
025200040820     c                   Clear                   comman
025300040820     c                   Movel(p)  cmd(1)        comman
025400040820     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
025500040910      *test sistema informativo di prova
025600040910     c                   if        %subst(knsif:7:1) = 'P'
025700040910     c                   Eval      %Subst(comman:30:10) = wlibp
025800040910     c                   else
025900040910     c                   Eval      %Subst(comman:30:10) = wlib
026000040910     c                   endif
026100040910
026200040820     c                   Eval      comman =
026300040820     c                             %trim(comman) + '/FNBLP01L)'
026400040820     c                   Call      'QCMDEXC'                            99
026500040820     c                   Parm                    comman
026600040820     c                   Parm                    lenght
026700040820     c                   If        *In99
026800040820     c                   seton                                        lr
026900040820     c                   Else
027000040820     c                   Open      Fnblp01l
027100040820     c                   EndIf
027200000406      *
027300040820      * Apro  Fnarb di filiale
027400040820     c                   Clear                   comman
027500040820     c                   Movel(p)  cmd(1)        comman
027600040820     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
027700040910      *test sistema informativo di prova
027800040910     c                   if        %subst(knsif:7:1) = 'P'
027900040910     c                   Eval      %Subst(comman:30:10) = wlibp
028000040910     c                   else
028100040910     c                   Eval      %Subst(comman:30:10) = wlib
028200040910     c                   endif
028300040910
028400040820     c                   Eval      comman =
028500040820     c                             %trim(comman) + '/FNARB01L)'
028600040820     c                   Call      'QCMDEXC'                            99
028700040820     c                   Parm                    comman
028800040820     c                   Parm                    lenght
028900040820     c                   If        *In99
029000040820     c                   seton                                        lr
029100040820     c                   Else
029200040820     c                   Open      Fnarb01l
029300040820     c                   EndIf
029400040820      *
029500000406     C                   Endsr
029600000407     o*--------------------------------------------------------------*
029700000407     o* Stampa
029800000407     o*-------------------------------------------------------------*
029900000413     OQsysPrt   E            Testa          1 01
030000000407     o                                           66 'Elenco eventi di messa in '
030100000407     o                                              'consegna da eliminare al '
030200000407     o                       datpul                 '    /  /  '
030300000411     O          E            totfle         1
030400000411     O                                              'Filiale:'
030500000411     O                       wsvfle        z   +  1
030600020704     O                                         +  1 'Totale eventi MIC deletati'
030700000411     O                       totmic        z   +  1
031000020704     O          E            finale         2
031100020704     O                                              ' In Generale'
031200020704     O                                         +  1 'Totale eventi MIC deletati'
031300020704     O                       totgen        z   +  1
031600040820      *----------------------------------------------------
031700040820** cmd
031800040820OVRDBF FILE(        ) TOFILE(
