000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200130729     ffifce00f  uf   e             disk
000300130725     FPRTF198   O    F  198        PRINTER oflind(*inof)
000400130724     d fifttds       e ds                  extname(fiftt00f)
000500130724     d fifceds       e ds                  extname(fifce00f)
000600130725     d newice          s              9  3
000700130725     d rrn             s             10  0
000800130724     d sqlpdr          s              7  0
000900130724     d sqlddc          s              8  0
001000130725     d totftt          s              9  3
001100130725     d totfce          s              9  3
001200130726     d $fine           s               n
001300131125     d salta_stampa    s              1    inz('S')
001400161114      **------------------------------------------------------------------------
001500161114     c     *entry        plist
001600161114     c                   parm                    trasmessi         1
001700161114     c                   parm                    dal               8 0
001800161114     c                   parm                    al                8 0
001900161114      **
002000130724     c                   if        dal <> 0 and al <> 0
002100130724      /free
002200080805         exec sql
002300130722          declare FTT cursor for
002400110629
002500130724          with selFTT as (select FTTPDR, FTTDDC,
002600130724          decimal(sum(fttitt + fttita + ftttpe + ftttbn + ftttim + fttmnt)
002700161114          , 9, 3) as TOTFTT              FROM fIFTT00f  WHERE FTTTSR = ' '
002800161114          and  fttddc between :DAL and :AL    and FTTFTR = :trasmessi
002900161114          GROUP BY fttpdr, fttddc),
003000130722
003100130724          selFCE as (select fcepdr, fceddc, decimal(sum(fceice), 9, 3)
003200161114          as totFCE from fifce00f   WHERE fceddc between :DAL and :AL
003300161114          GROUP BY fcepdr, fceddc )
003400110629
003500161114          select  fcepdr, fceddc, totfce, totftt from SELFCE  join SELFTT
003600161114            ON   fttpdr=fcepdr and fttddc=fceddc
003700130725          WHERE (totFce-totFTT >1 or totFce - totFTT<-1);
003800130724
003900130724         exec sql open ftt;
004000110623
004100130724         dow not $fine;
004200130724          exec sql fetch next from ftt into
004300130729          :sqlpdr, :sqlddc, :totfce, :totftt;
004400130724
004500130724          if sqlcod = 100 or sqlcod < 0;
004600130724           $fine = *on;
004700130724           leave;
004800130724          endif;
004900130724         exsr spalma;
005000130724         enddo;
005100130724
005200130724         exec sql close ftt;
005300130724      /end-free
005400161114
005500130724     c                   end
005600130722     c                   seton                                        lr
005700161114      *-------------------------------------------------------------------------**
005800130724     c     spalma        begsr
005900161114      *-------------------------------------------------------------------------**
006000130724     C/EXEC SQL
006100130725     C+ DECLARE A1 CURSOR FOR SELECT FCEPDR, FCEDDC, FCETSR, FCENDC,
006200130725     C+ FCECCE, FCEICE, FCEAAS, FCELNP, FCENRS, FCENSP, rrn(fifce00f),
006300130725     C+ case when :totfce <> 0 then decimal((fceice * :totftt)/:totfce,
006400130725     C+ 9, 3) else 0 end NEWICE FROM fifce00f WHERE
006500130725     C+ fcepdr = :sqlpdr and fceddc = :sqlddc
006600130724     C/END-EXEC
006700130724
006800130724     C/EXEC SQL
006900130724     C+ OPEN A1
007000130724     C/END-EXEC
007100130724     C                   DOU       SqlCod <> 0
007200130724     C/EXEC SQL
007300130725     C+ Fetch Next From A1 Into :fcepdr, :fceddc, :fcetsr,
007400130725     C+ :fcendc, :fcecce, :fceice,
007500130725     C+ :fceaas, :fcelnp, :fcenrs, :fcensp, :rrn, :newice
007600130724     C/END-EXEC
007700130724     C                   SELECT
007800130724     **
007900130724     C                   WHEN      SqlCod = 100
008000130724     c                   leave
008100130724     C                   WHEN      SqlCod < 0
008200130724     C                   seton                                        H1
008300130724     c                   leave
008400161114      *
008500130724     C                   OTHER
008600161114      *
008700130729     c                   if        newice <> fceice
008800161114      *
008900130729     c     rrn           chain     Fifce00f
009000130729     c                   if        %Found(Fifce00f)
009100130725     C* nuovo importo sulla riga
009200130729     C                   z-add     newice        fceice
009300130729     c                   Update    fifce000
009400130729     c                   End
009500161114      *
009600131125     c                   if        salta_stampa <>'S'
009700130725     c                   except    riga
009800131125     c                   end
009900161114      *
010000130729     c                   End
010100130724     C                   ENDSL
010200130724      *
010300130724     C                   ENDDO
010400161114      *
010500161114      *   può rimanere una differenza fra i 2 valori e la differenza
010600161114      *  viene riportata sull'ultimo record aggiornato.
010700161114     c                   z-add     fcepdr        pdr               7 0
010800161114     c                   z-add     fceddc        ddc               8 0
010900161114     c                   z-add     0             diffe             9 3
011000130724     C/EXEC SQL
011100130724     C+ Close A1
011200130724     C/END-EXEC
011300161114      *
011400161114     C*       RITOTALIZZA
011500161114      /free
011600161114         exec sql
011700161114          declare DIF cursor for
011800161114
011900161114          WITH
012000161114          FTTFILE as (select FTTPDR, FTTDDC,
012100161114          decimal(sum(fttitt + fttita + ftttpe + ftttbn + ftttim + fttmnt)
012200161114          , 9, 3) as TOTFTT              FROM fIFTT00f  WHERE FTTTSR = ' '
012300161114          and  fttddc = :ddc  and fttpdr = :pdr and FTTFTR = :trasmessi
012400161114          GROUP BY fttpdr, fttddc),
012500161114
012600161114          FCEFILE as (select fcepdr, fceddc, decimal(sum(fceice), 9, 3)
012700161114          as totFCE from fifce00f   WHERE fceddc = :ddc and fcepdr = :pdr
012800161114          GROUP BY fcepdr, fceddc )
012900161114
013000161114          select  fcepdr, fceddc, ABS(totftt-totfce)
013100161114           from FCEfile  join FTTfile
013200161114            ON   fttpdr=fcepdr and fttddc=fceddc;
013300161114
013400161114         exec sql open DIF;
013500161114         exec sql fetch next from DIF into
013600161114                 :sqlpdr, :sqlddc, :diffe;
013700161114
013800161114          if sqlcod = 0 and diffe<>0;
013900161114             exsr somma_diffe;
014000161114          endif;
014100161114
014200161114         exec sql close DIF;
014300161114      /end-free
014400130724     C*
014500130724     c                   endsr
014600161114      *-------------------------------------------------------------------------**
014700161114     c     somma_DIFFE   begsr
014800161114      *-------------------------------------------------------------------------**
014900161114     C*  sull'ultimo record aggiornato aggiunge la differenza
015000161114      *
015100161114     c     rrn           chain     Fifce00f
015200161114     c                   if        %Found(Fifce00f)
015300161114     C                   add       diffe         fceice
015400161114     c                   Update    fifce000
015500161114     c                   End
015600161114      *
015700161114     c                   endsr
015800161114     C* --------------------------------------------------------------- *
015900130725     oPRTF198   e            riga        1
016000130725     O                                            3 'AUT'
016100130725     o                       fcepdr        Z     +1
016200130725     O                                           +1 'DATA'
016300130725     o                       fceddc        Z     +1
016400130725     O                                           +1 'TSR'
016500130725     o                       fcetsr              +1
016600130725     O                                           +1 'SPED'
016700130725     o                       fceaas        Z     +1
016800130725     o                       fcelnp        Z     +1
016900130725     o                       fcenrs        Z     +1
017000130725     o                       fcensp        Z     +1
017100130725     O                                           +1 'VOCE'
017200130725     o                       fcecce              +1
017300130725     O                                           +1 'IMP OLD'
017400130725     o                       fceice        4     +1
017500130725     O                                           +1 'IMP NEW'
017600130725     o                       newice        4     +1
017700130725     O                                           +1 'TOT FTT'
017800130725     o                       totftt        4     +1
017900130725     O                                           +1 'TOT FCE'
018000130725     o                       totfce        4     +1
