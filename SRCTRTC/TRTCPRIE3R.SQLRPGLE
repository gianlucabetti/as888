000100171110     h decedit('0,') option(*nodebugio)
000101171110
000102171110      //--------------------------------------------------------------------
000800080423     d fiprdds       e ds                  extname(fiprd00r)
000900170526     d fiprdrieds    e ds
000901171110
001900171110      //--------------------------------------------------------------------
002000070214     c     *entry        plist
002100080423     c                   parm                    fiprdds
002200171110
002201171110       fiprdrieds = prddati;
002202171110
002203171110       exec sql
002205171110       delete from fipdo00t where pdofgs = :§riefgs and pdondc = :§riendc;
002206171110
002207171110       exec sql
002208171110       delete from fipro10f where profgs = :§riefgs and prondc = :§riendc;
002209171110
003315171110
003316171110       *inrt = *on;
