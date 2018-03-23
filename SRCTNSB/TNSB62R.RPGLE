000100000406      *------------------------------------------------------------------------*
000200061018      * Pulizia Eventi senza Bolle          Lanciato da SEDE
000300000406      *------------------------------------------------------------------------*
000400000406     H DECEDIT('0,') DATEDIT(*DMY.)
000500000406      *------------------------------------------------------------------------*
000600000406      * D A T A      B A S E
000700000406      *------------------------------------------------------------------------*
000800131031     FFNEVB01L  iF   E           K DISK
000801131029     FFNEVB22L  UF   E           K DISK
000900000406     F*-------
001000000406     FTITAS30C  IF   E           K DISK
001600040820     Ffnblp01L  IF   E           K DISK    usropn
001700040820     Ffnarb01L  IF   E           K DISK    usropn
001800040820     F*-------
001900000407     FQSYSPRT   O    F  132        PRINTER oflind(*inof)
002000020701      *------------------------------------------------------------------------*
002100000406      * D s
002200000406      *------------------------------------------------------------------------*
002300000406     D* ds architettura e passaggio parametri
002400000406     D KPJBA         E DS
002401061018
002900000406     D* ds x calcolo data pulizia (conversione data odierna in gg)
003000000406     D WLBDAT          DS
003100000406     D  G02DAT                 1      8  0
003200000406     D  G02INV                 9     16  0
003300000406     D  G02ERR                17     17
003400000406     D  G02TGI                18     22  0
003401061018
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
005500000406      * Prima lettura (Reperisco codice filiale)
005601131031     C     *start        setll     fnevb22l
005602131031     C                   read      fnevb22l
005700061018      * Loop lettura
005800131031     C                   DOW       not %EoF(fnevb22l)
005802061018      * aggancio la bolla
005803131031     C                   Z-add     Evb2aas       Kaas
005804131031     C                   Z-add     Evb2Lnp       Klnp
005805131031     C                   Z-add     Evb2nrs       Knrs
005806131031     C                   Z-add     Evb2nsp       Knsp
005807131031     C     Ktas          Chain     fnevb01l                           32
005809061018      * spegne gli indicatori di tipo record del file combinato
005810061018     C                   setoff                                       505152
005811131031     C   32Ktas          Chain     Titas30C                           32
005812061018
005813061018      * se non c'è su TAS proseguo
005814061018     C                   if        *in32
005815061018
005816061018      * verifico anche non esistano ARB e BLP altrimenti non cancello
005817061018     C     Ktas          Chain     fnblp01l                           33
005818061018     C   33Ktas          Chain     fnarb01l                           33
005819061018
005820061018     c                   if        *in33
005825131029     c                   delete    fnevb22l
005826131031     C                   add       1             totgen
005828061018     C                   end
005829061018      *
005830061018     C                   endIF
005831061018
006800061018      * Eseguo nuova lettura
007001131031     C                   read      fnevb22l
007100061018     C                   EndDO
007101061018
007200020704     C   Of              except    Testa
007300020704     C                   except    finale
007400000406      * Fine elaborazione
007500000406     C     Fine          TAG
007600000406     C                   SETON                                        LR
021800000406      *------------------------------------------------------------------------*
021900000406      * *inzsr: Definizione parametri e klist
022000000406      *------------------------------------------------------------------------*
022100000406     C     *inzsr        Begsr
022200000406      *
022300000406     C     *entry        plist
022400000406     C                   parm                    kpjba
022401061018      *
023300000406     C     kTas          klist
023400000406     C                   kfld                    Kaas
023500000406     C                   kfld                    Klnp
023600000406     C                   kfld                    Knrs
023700000406     C                   kfld                    Knsp
023701131029     C     Kevb2f        KLIST
023702131029     C                   KFLD                    EVBAAS
023703131029     C                   KFLD                    EVBLNP
023704131029     C                   KFLD                    EVBNRS
023705131029     C                   KFLD                    EVBNSP
023706131029     C                   KFLD                    EVBDTV
023707131029     C                   KFLD                    EVBORV
023708131029     C                   KFLD                    EVBCEV
023800000406      * Definizione campi
024400000406     C     *like         define    tasaas        Kaas
024500000406     C     *like         define    taslnp        Klnp
024600000406     C     *like         define    tasnrs        Knrs
024700000406     C     *like         define    tasnsp        Knsp
024900000406     C     *like         define    evbdev        oggi
025001061018
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
029402061018      * Reperisco la data del giorno e ....
029403061018     C                   TIME                    Whhdat           14 0
029404061018      * .. la giro nel formato aaaa-mm-gg reperendo i gg.
029405061018     C                   Clear                   Wlbdat
029406061018     C                   MOVE      whhdat        g02dat
029407061018     C                   call      'XSRDA8'
029408061018     C                   parm                    Wlbdat
029409061018     C                   move      G02inv        oggi
029410061018      *
029411061018      * Imposto filiale immissione evento a 0
029413061018     C                   Seton                                        of
029414061018     C* contatori genrali
029415061018     C                   z-add     0             totgen            7 0
029416061018     C*
029500000406     C                   Endsr
029600000407     o*--------------------------------------------------------------*
029700000407     o* Stampa
029800000407     o*-------------------------------------------------------------*
029900000413     OQsysPrt   E            Testa          1 01
030000061018     o                                           66 'Elenco eventi da eliminare'
030100061018     o                                              ' senza bolle sede/filiale'
031000020704     O          E            finale         2
031100020704     O                                              ' In Generale'
031200061018     O                                         +  1 'Totale eventi deletati'
031300020704     O                       totgen        z   +  1
031600040820      *----------------------------------------------------
031700040820** cmd
031800040820OVRDBF FILE(        ) TOFILE(
