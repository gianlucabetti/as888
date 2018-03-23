000100130805      *===============================================================*
000200130805      * TNSB31R    * Rigenerazione immagine LdV per AUTISTA           *
000300130805      *===============================================================*
000400130805
000500130805     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600130805
000700130805      *===============================================================*
000800130805
000900130805     fTILAC00F  o    e             disk
001000130805     fTITAS30C  If   e           k disk
001100040721
001200130805      *===============================================================*
001300130805
001400130805      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
001500130805      *
001600130805     d KPJBA         e ds
001700130805      * - Parametri x Controllo profilo utenti
001800130805     d TIBS34DS      e ds                  inz
001900130805      * - Ds di riferimento al file esterno AZUTE00F
002000130805     d AZUTEDS       e ds                  extname(AZUTE00F)
002100130805      * - Ds per dati organigramma
002200130805     d DDatiUte      e ds
002300130805      * - Ds per campo TASFLO del file TITAS31C
002400130805     d dTASflo       e ds                  inz
002500130805
002600130805     d FIFTDds       e ds                  extname(FIFTD00F)
002700130805
002800130805     d dstb          e ds                  inz
002900130805
003000130625     d TNSB30ds      e ds                  inz
003100130805
003200130805     d DS_Time1        ds            14    inz
003300130805     d   dsT1_hms                     6  0 inz
003400130805     d   dsT1_dmy                     8  0 inz
003500130805     d DS_Time2        ds            14    inz
003600130805     d   dsT2_ymd                     8  0 inz
003700130805     d   dsT2_hms                     6  0 inz
003800130805      *
003900130805      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
004000130805      *
004100130805      * - Campi di comodo
004200130805     d WdateISO        s               d   datfmt(*ISO) inz
004300130805     d Wtime           s             14  0              inz
004400130805     d wEnD            s               n   inz(*off)
004500130805     d Wftdpdr         s              7    inz
004600130805     d Wftdddc         s              8    inz
004700130805     d Wftdaas         s              4    inz
004800130805     d Wftdlnp         s              3    inz
004900130805     d Wftdnrs         s              2    inz
005000130805     d Wftdnsp         s              7    inz
005100130805
005200130805      *===============================================================*
005300130805      *  RIEPILOGO INDICATORI                                         *
005400130805      *---------------------------------------------------------------*
005500130805      *                                                               *
005600130805      *===============================================================*
005700130805
005800130805     c     *Entry        plist
005900130805     c                   parm                    KPJBA
006000130805     c                   movel     KPJBU         TNSB30ds
006100130805      /free
006200130805        exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006300130805      /end-free
006400130805
006500130805      * Operazioni iniziali
006600130805     c                   exsr      OperazIniz
006700130805
006800130805     c                   eval      wEnd = *off
006900130805
007000130805      /free
007100130805         exec sql
007200130805          DECLARE FTD cursor for
007300130805          SELECT   * from FIFTD06L
007400130805          WHERE    FTDpdr = :ISB30pdr and FTDddc between
007500130805                   :ISB30dtd and :ISB30dta and FTDtsr = 'C' and
007600130805                   FTDcmc <> 'P'
007700130805          ORDER BY FTDpdr, FTDddc, FTDtsr, FTDndc;
007800130805
007900130805          exec sql
008000130805           OPEN FTD;
008100130805
008200130805          DOW not wEnD;
008300130805            exec sql
008400130805             FETCH next from FTD into: FIFTDds;
008500130805             IF sqlcod = 100 or sqlcod < 0;
008600130805               wEnD = *on;
008700130805               leave;
008800130805             ENDIF;
008900130805
009000130805      /end-free
009100130805
009200130805      * aggancio la spedizione
009300130805     c     Ktasftd       chain     titas30c
009400130805     c                   if        %found(titas30c)
009500130805     c                   movel     TASflo        dTASflo
009600130805      * controllo corrispondenza distina e autista
009700130805if  2c                   if        ftdndc <> tasndc
009800130805     c                   iter
009900130805     c                   end
010000130805if  2c                   if        ftdpdr <> taspdc
010100130805     c                   iter
010200130805     c                   end
010300130805      * elaborazione delle sole LdV con immagine
010400130805if  2c                   if        §FLOiml = ' '
010500130805     c                   iter
010600130805     c                   end
010700130805
010800130805     c                   exsr      Wrt_TILAC
010900130805     c                   endif
011000130805
011100130805      /free
011200130805          ENDDO;
011300130805
011400130805        exec sql
011500130805         CLOSE FTD;
011600130805      /end-free
011700130805
011800130805      * Fine
011900130805     c                   eval      *inLR = *on
012000130805
012100130805      *---------------------------------------------------------------*
012200130805      * Operazioni iniziali                                           *
012300130805      *---------------------------------------------------------------*
012400130805     c     OperazIniz    BEGSR
012500130805
012600130805     c     Ktasftd       klist
012700130805     c                   kfld                    ftdaas
012800130805     c                   kfld                    ftdlnp
012900130805     c                   kfld                    ftdnrs
013000130805     c                   kfld                    ftdnsp
013100130805
013200130805     c     Kftd          klist
013300130805     c                   kfld                    ISB30pdr
013400130805     c                   kfld                    FTDddc
013500130805     c                   kfld                    ftdtsr
013600130805     c                   eval      ftdtsr = 'C'
013700130805
013800130805      * Reperisco dati job
013900130805     c                   exsr      DatiJob
014000130805
014100130805      * Impostazione del campo LAcTIM = aaaa/mm/gg+hh:mm:ss
014200130805     c                   time                    Wtime
014300130805     c                   movel     Wtime         DS_Time1
014400130805     c                   eval      dsT2_hms   = dsT1_hms
014500130805     c     *eur          movel     dsT1_dmy      WdateISO
014600130805     c                   move      WdateISO      dsT2_ymd
014700130805
014800130805     c                   ENDSR
014900130805
015000130805      *---------------------------------------------------------------*
015100130805      * Reperimento Dati del job (Utente/Operativi)                   *
015200130805      *---------------------------------------------------------------*
015300130805     c     DatiJob       BEGSR
015400130805
015500130805     c     *dtaara       define    §azute        azuteds
015600130805     c     *dtaara       define    §datiute      ddatiute
015700130805
015800130805     c                   in(E)     *dtaara
015900130805     c                   IF        %ERROR or RSUT = *blanks
016000130805     c                   clear                   Tibs34Ds
016100130805     c                   call      'TIBS34R'
016200130805     c                   parm                    Tibs34Ds
016300130805     c                   in        *dtaara
016400130805     c                   ENDIF
016500130805
016600130805     c                   ENDSR
016700130805
016800040721      *---------------------------------------------------------------*
016900050317      * Registrazione dati nel file TILAC00F x cliente                *
017000040721      *---------------------------------------------------------------*
017100050317     c     Wrt_TILAC     BEGSR
017200130805
017300130625     c                   movel     ftdpdr        Wftdpdr
017400130625     c                   movel     ftdddc        Wftdddc
017500130805     c                   movel     ftdaas        Wftdaas
017600130805     c                   movel     ftdlnp        Wftdlnp
017700130805     c                   movel     ftdnrs        Wftdnrs
017800130805     c                   movel     ftdnsp        Wftdnsp
017900040721      *
018000050317     c                   clear                   TILAC000
018100050317     c                   move      TASaas        LACaas
018200050317     c                   move      TASlnp        LAClnp
018300050317     c                   move      TASnrs        LACnrs
018400050317     c                   move      TASnsp        LACnsp
018500130805     c                   movel     'CLI'         LACcop
018600040721      *
018700040721      * Impostazione nuovi dati
018800050317     c                   eval      LACtim  = DS_Time2
018900050317     c                   eval      LACela  = *zeros
019000130805     c                   eval      LACdir  = 'A/' + %subst(ISB30idjob:1:8) +
019100130805     c                             '/' + wftdpdr + '/' + wftdddc
019200130625     c                   eval      LACtpi  ='PDF'
019300130625     c                   eval      LACnim  = wftdaas+wftdlnp+wftdnrs+wftdnsp
019400130625     c                   eval      lactad = 'N'
019500090310      * imposto i nuovi campi di TILAC
019600090310     c                   eval      lactela = 'S'
019700130625     c                   eval      lacksc = 1020001
019800130625     c                   eval      lacksu = 1020001
019900130805     c                   eval      lacidjob = ISB30idjob
020000040721      *
020100050317     c                   WRITE     TILAC000
020200040721      *
020300040721     c                   ENDSR
