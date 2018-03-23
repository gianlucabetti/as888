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
001400130724     c                   clear                   dal               8 0
001500130722     c                   clear                   al                8 0
001600130722     C/EXEC SQL
001700130724     c+   select MIN(FTTDDC), MAX(fTTDDC) INTO :DAL, :AL
001800130722     c+   from fIFTT00f WHERE FTTTSR = ' '  and
001900130722     c+   FTTFTR = ' '
002000130722     C/END-EXEC
002100130724     c                   if        dal <> 0 and al <> 0
002200130724      /free
002300080805         exec sql
002400130722          declare FTT cursor for
002500110629
002600130724          with selFTT as (select FTTPDR, FTTDDC,
002700130724          decimal(sum(fttitt + fttita + ftttpe + ftttbn + ftttim + fttmnt)
002800130729          , 9, 3) TOTFTT
002900130722          from fIFTT00f WHERE FTTTSR = ' '  and  fttddc between :dal and :al
003000130726          and FTTFTR = ' ' group by fttpdr, fttddc),
003100130722
003200130724          selFCE as (select fcepdr, fceddc, decimal(sum(fceice), 9, 3)
003300130724          totFCE from fifce00f
003400130722          WHERE
003500130722          fceddc between :DAL and :AL group by fcepdr, fceddc )
003600110629
003700130729          select  fcepdr, fceddc, totfce, totftt from
003800130724          selfce  join selftt on
003900130725          fttpdr=fcepdr and fttddc=fceddc
004000130725          WHERE (totFce-totFTT >1 or totFce - totFTT<-1);
004100130724
004200130724         exec sql open ftt;
004300110623
004400130724         dow not $fine;
004500130724          exec sql fetch next from ftt into
004600130729          :sqlpdr, :sqlddc, :totfce, :totftt;
004700130724
004800130724          if sqlcod = 100 or sqlcod < 0;
004900130724           $fine = *on;
005000130724           leave;
005100130724          endif;
005200130724         exsr spalma;
005300130724         enddo;
005400130724
005500130724         exec sql close ftt;
005600130724      /end-free
005700130724     c                   end
005800130722     c                   seton                                        lr
005900130724
006000130724     c     spalma        begsr
006100130724
006200130724     C/EXEC SQL
006300130725     C+ DECLARE A1 CURSOR FOR SELECT FCEPDR, FCEDDC, FCETSR, FCENDC,
006400130725     C+ FCECCE, FCEICE, FCEAAS, FCELNP, FCENRS, FCENSP, rrn(fifce00f),
006500130725     C+ case when :totfce <> 0 then decimal((fceice * :totftt)/:totfce,
006600130725     C+ 9, 3) else 0 end NEWICE FROM fifce00f WHERE
006700130725     C+ fcepdr = :sqlpdr and fceddc = :sqlddc
006800130724     C/END-EXEC
006900130724
007000130724     C/EXEC SQL
007100130724     C+ OPEN A1
007200130724     C/END-EXEC
007300130724     C                   DOU       SqlCod <> 0
007400130724     C/EXEC SQL
007500130725     C+ Fetch Next From A1 Into :fcepdr, :fceddc, :fcetsr,
007600130725     C+ :fcendc, :fcecce, :fceice,
007700130725     C+ :fceaas, :fcelnp, :fcenrs, :fcensp, :rrn, :newice
007800130724     C/END-EXEC
007900130724     C                   SELECT
008000130724     **
008100130724     C                   WHEN      SqlCod = 100
008200130724     c                   leave
008300130724     C                   WHEN      SqlCod < 0
008400130724     C                   seton                                        H1
008500130724     c                   leave
008600130724     C                   OTHER
008700130729     c                   if        newice <> fceice
008800130729     c     rrn           chain     Fifce00f
008900130729     c                   if        %Found(Fifce00f)
009000130725     C* nuovo importo sulla riga
009100130729     C                   z-add     newice        fceice
009200130729     c                   Update    fifce000
009300130729     c                   End
009400131125     c                   if        salta_stampa <>'S'
009500130725     c                   except    riga
009600131125     c                   end
009700130729     c                   End
009800130724     C                   ENDSL
009900130724      *
010000130724     C                   ENDDO
010100130724      *
010200130724     C/EXEC SQL
010300130724     C+ Close A1
010400130724     C/END-EXEC
010500130724     C*
010600130724     C*
010700130724     c                   endsr
010800130725     oPRTF198   e            riga        1
010900130725     O                                            3 'AUT'
011000130725     o                       fcepdr        Z     +1
011100130725     O                                           +1 'DATA'
011200130725     o                       fceddc        Z     +1
011300130725     O                                           +1 'TSR'
011400130725     o                       fcetsr              +1
011500130725     O                                           +1 'SPED'
011600130725     o                       fceaas        Z     +1
011700130725     o                       fcelnp        Z     +1
011800130725     o                       fcenrs        Z     +1
011900130725     o                       fcensp        Z     +1
012000130725     O                                           +1 'VOCE'
012100130725     o                       fcecce              +1
012200130725     O                                           +1 'IMP OLD'
012300130725     o                       fceice        4     +1
012400130725     O                                           +1 'IMP NEW'
012500130725     o                       newice        4     +1
012600130725     O                                           +1 'TOT FTT'
012700130725     o                       totftt        4     +1
012800130725     O                                           +1 'TOT FCE'
012900130725     o                       totfce        4     +1
