000100051222      /TITLE legge fiftt/fiatt/fictt UNA VOLTA AL MESE IN SEDE
000200170426
000300051129     ftntud01l  uf a e           k disk
000400170530     fndreg01l  if   e           k disk
000500170530     fanutn02l  if   e           k disk
000600170530     fazute01l  if   e           k disk
000700170530     fwfttnragf if   e             disk    usropn
000800170531     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900170530      *-
001000051129     dkpjba          e ds
001100051229     dtntudtads      e ds
001200051129     dtntu05ds       e ds
001300170427      **
001400170530     d fgs             s              3  0 inz
001500051214     d mult            s                   like(tudmult)
001600170427      *
001700170530     d dtaiso          s               d   inz
001800170427     d dtada_iso       s               d   datfmt(*iso)
001900170427     d dtaa_iso        s               d   datfmt(*iso)
002000170531      *
002100170531      * Schiere per totalizzare a livello di DATA/FILIALE
002200170531     D DATFIL          S             13    DIM(9999) INZ
002300170531     D ContaNR         S              9  0 DIM(9999) INZ
002400170531     D NUM             S              5  0 INZ
002500170531      *
002600170530      *--------------------------------------------
002700170530     C     *ENTRY        PLIST
002800170530     C                   PARM                    kpjba
002900170530     c                   movel     kpjbu         tntu05ds
003000170531     c                   seton                                        of
003100170530      *
003200170530      *  Da data a Data in formato *ISO
003300170427     c     *iso          move      t05dti        dtada_iso
003400170427     c     *iso          move      t05dtf        dtaa_iso
003500170427      *
003600051222     **?calcolo nr. fatture autotrasp.
003700051219     c                   if        t05mult = ' ' or t05mult = '290'
003800170530      *
003900170530     c* cancello il file di lavoro per il nr. registrazione del fiftt
004000170530     C/EXEC SQL
004100170530     C+ Delete wfttnragf
004200170530     C/END-EXEC
004300170530      *
004400170530      * creo il file di lavoro con il nr. assoluto
004500170530     C/EXEC SQL
004600170530     C+ insert into
004700170530     C+ wfttnragf (wnra) SELECT dec(substr(fttflr, 2, 9), 9, 0) FROM
004800170530     C+ fiftt00f WHERE substr(fttflr, 2, 9) <> ' ' and fttnff <> 0 and
004900170530     C+ fttnff <> 999999999 and ftttsr = ' ' GROUP BY dec(substr(fttflr,
005000170530     C+ 2, 9), 9, 0)
005100170530     C/END-EXEC
005200060119     c                   eval      mult = '290'
005300060119     c                   exsr      srdatafat
005400051219     c                   end
005500170427      **
005600051222     **?calcolo nr. fatture aff/defl
005700051222     c                   if        t05mult = ' ' or t05mult = '300'
005800170530     C/EXEC SQL
005900170530     C+ Delete wfttnragf
006000170530     C/END-EXEC
006100170530      *
006200170530      * creo il file di lavoro con il nr. assoluto
006300170530     C/EXEC SQL
006400170530     C+ insert into
006500170530     C+ wfttnragf (wnra) SELECT attnra FROM
006600170530     C+ fiatt00f WHERE attnra  <> 0
006700170530     C+ GROUP by 0, attnra
006800170530     C/END-EXEC
006900060119     c                   eval      mult = '300'
007000060119     c                   exsr      srdatafat
007100051222     c                   end
007200170427      **
007300051222     **?calcolo nr. fatture coop
007400051222     c                   if        t05mult = ' ' or t05mult = '310'
007500170530     C/EXEC SQL
007600170530     C+ Delete wfttnragf
007700170530     C/END-EXEC
007800170530
007900170530      * creo il file di lavoro con il nr. assoluto
008000170530     C/EXEC SQL
008100170530     C+ insert into
008200170530     C+ wfttnragf (wnra) SELECT cttnra FROM
008300170530     C+ fictt00f WHERE cttnra  <> 0
008400170530     C+ GROUP by 0, cttnra
008500170530     C/END-EXEC
008600051222     c                   eval      mult = '310'
008700060119     c                   exsr      srdatafat
008800051222     c                   end
008900170426      ***
009000051229     c* aggiorno data ultima elaborazione nella tabella TUP
009100051229     c                   clear                   tntudtads
009200051229     c                   eval      §DTAazn = kcoaz
009300051229     c                   eval      §DTAdta = t05dtf
009400051229     c                   call      'TNTUDTAR'
009500051229     c                   parm                    tntudtads
009600051214     c*
009700051214     c                   seton                                        lr
009800170530      **********************************************************************
009900170530      *?Controlla se la registrazione è utile al conteggio
010000170530      **********************************************************************
010100060119     c     srdatafat     begsr
010200170530      *
010300170531     c                   clear                   DATFIL
010400170531     c                   clear                   ContaNR
010500170531     c                   clear                   NUM
010600170531      *
010700170530     c                   open      wfttnragf
010800170530      *
010900170530      ** legge i numeri registrazione appena creati
011000051219     C                   DO        *hival
011100170530     c                   read      wfttnragf
011200170530      *   a EoF Esce
011300170530     C                   if        %EoF(wfttnragf)
011400051219     c                   leave
011500170530     c                   end
011600170530      *
011700170530      ***   Nr.Registrazione
011800170530     c                   eval        REGSYS =0
011900170530     c                   eval        REGNRASREG =  WNRA
012000170530     c     keyREG        chain     ndreg01l
012100170530     c                   if        %Found(ndreg01l)
012200170531      *
012300170530      **  con UTENTE
012400170530     c     reguteimm     chain     anutn02l
012500170530     c                   if        %Found(anutn02l)
012600170531      *
012700170530      **  con PROFILO prende la Filiale
012800170530     c     utnprf        chain     azute01l
012900170530     c                   if        %Found(azute01l)
013000170530      ***
013100170427     **?Se passato p.o. o la DATA è fuori dal Range
013200170530     C                   if        t05fil <> 0 and uteFIL <> t05fil
013300170522     c                               or
013400170530     c                             REGDTIMM < dtada_iso
013500170522     c                               or
013600170530     c                             REGDTIMM > dtaa_iso
013700170522     c                   iter
013800170522     c                   end
013900170427      **
014000170530      **    CONTA x Data e FILIALE (+1)
014100170530     c                   move      REGDTIMM      dtaiso
014200170530     c                   move      utefil        fgs
014300170531      **
014400170531     **?Totalizza per Data/Filiale
014500170531     c                   exsr      TOT_DATFIL
014600170427      **
014700170530     c                   end
014800170530     c                   end
014900170530     c                   end
015000170530      **
015100051219     C                   ENDDO
015200170530      *
015300170530     c                   close     wfttnragf
015400170531      *
015500170531      * aggiorna il D.B. in funzione delle schiere generate
015600170531     c                   exsr      AggDB
015700170531      *
015800170531     c                   endsr
015900170531     c**********************************************************************
016000170531      ** Totalizza per Data Filiale
016100170531     c**********************************************************************
016200170531     c     TOT_DATFIL    begsr
016300170531      **
016400170531      ** Data + Filiale
016500170531     c                   movel     dtaiso        GIOFIL           13
016600170531     c                   move      fgs           GIOFIL
016700170531     c                   z-add     1             XX
016800170531     c     GIOFIL        lookup    datfil(XX)                             21
016900170531      *  cerca uno vuoto
017000170531     c                   if          not *IN21
017100170531     c                   z-add     1             XX
017200170531     c     *blank        lookup    datfil(XX)                             21
017300170531     c   21              movel     dtaiso        datfil(XX)
017400170531     c   21              move      fgs           datfil(XX)
017500170531     c                   end
017600170531      *
017700170531     c                   if          *IN21
017800170531     c                   add       1             ContaNR(XX)
017900170531     c                   eLSe
018000170531      **
018100170531      *      segnala problema che NON ci sono più spazi in SCHIERA
018200170531     C   OF              EXCEPT    TESTA
018300170531     C   OF              SETOFF                                       OF
018400170531     C                   EXCEPT    anomalia
018500170531      **
018600170531     c                   end
018700170531      **
018800051219     c                   endsr
018900051214     c**********************************************************************
019000170531      ** Aggiorna i contatori
019100170531     c**********************************************************************
019200170531     c     aggDB         begsr
019300170531      **
019400170531     c                   eval      num = %elem(datfil)
019500170531      **
019600170531     c                   DO        num           XX                5 0
019700170531      **  esce appena non Ha più nulla nelle schiere
019800170531     c                   if        datfil(xx) = *blank
019900170531     c                   leave
020000170531     c                   end
020100170531      **
020200170531      **  aggancia il record
020300170531     c                   movel     datfil(XX)    dtaiso
020400170531     c                   move      datfil(XX)    fgs
020500051129     c     keytud        chain     tntud01l
020600051129     c                   if        %found(tntud01l)
020700170531     c                   eval      tudqta = ContaNR(XX)
020800051129     c                   update    tntud000
020900051129     c                   else
021000051214     c                   eval      tudmult = mult
021100051130     c                   eval      tuddta  = dtaiso
021200170530     c                   eval      tudfil  = fgs
021300170531     c                   eval      tudqta = ContaNR(XX)
021400051129     c                   clear                   tudcli
021500051129     c                   write     tntud000
021600051129     c                   end
021700170427      **
021800170531     c                   endDO
021900170531      **
022000051129     c                   endsr
022100051129     c**********************************************************************
022200051129     c     *inzsr        begsr
022300051129     c**********************************************************************
022400170531      ***
022500051129     c     keytud        klist
022600051214     c                   kfld                    mult
022700051130     c                   kfld                    dtaiso
022800170530     c                   kfld                    fgs
022900170530      ***
023000170530     c     keyREG        klist
023100170530     c                   kfld                    REGSYS
023200170530     c                   kfld                    REGNRASREG
023300170427      ***
023400051129     c                   endsr
023500170531     C**************************************************************************
024200170531      ***  Stampa di AVVISO  mancato conteggio
024300170531     C**************************************************************************
024400170531     OQSYSPRT   E            TESTA            02
024500170531     O                                           41 'Anomalie nel pgm TNTU12R'
024600170531     O                       PAGE          Z    132
024700170531     O          E            anomalia    1
024800170531     O                                           +0 'Non è stato possibile aggi-
024900170531     O                                              ornare il moltiplicatore : '
025000170531     O                       mult                +1
025100170531     O                                           +1 'P.O.'
025200170531     O                       fgs                 +1
025300170531     O                                           +1 'Data'
025400170531     O                       dtaiso              +1
025500170531     O          E            anomalia    1
025600170531     O                                           +0 'SUPERATA capienza SCHIERA -
025700170531     O                                              a programma. Segnalare il P-
025800170531     O                                              ROBLEMA al programmatore'
