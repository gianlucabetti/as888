000100161202     Fazorg01L  uF   e           k disk
000200161202     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000300021118
000400161202     d Og129         e ds                  inz
000500021118
000600161202     c     *entry        plist
000700161202     c                   parm                    tipo              1
000800161202     c                   parm                    AGG               3
000900161202     c
001000161202     c* Aggiunge o toglie   1 ORA
001100161202    1c                   if        tipo= 'P' or  tipo= 'M'
001200161202
001300161202     C                   CLEAR                   TIPOOP           14
001400161202     C                   CLEAR                   TIPOag           10
001500161202     c                   if        tipo ='P'
001600161202     c                   eval      tipoop='AGGIUNGI 1 ora'
001700161202     C                   ELSE
001800161202     c                   eval      tipoop='TOGLI    1 ora'
001900161202     C                   ENDIF
002000161202
002100161202     c                   if        %parms=2 and agg='AGG'
002200161202     c                   eval      tipoag='- AGGIORNA'
002300161202     C                   ELSE
002400161202     c                   eval      tipoag='- SIMULA  '
002500161202     C                   ENDIF
002600161202     C
002700161202     C                   EXCEPT    TESTA
002800161202
002900161202     c     *loval        setll     azorg01l
003000161202     c                   read      azorg01l
003100161202     c
003200161202    2c                   dow       not %eof(azorg01l)
003300161202    3c                   if        orgfag='F' or orgfag='A'
003400161202     c* Escludo estero
003500161202     c                   clear                   V02orfmd          4 0
003600161202     c
003700161202     c                   if        orgfl1<>'E'
003800161202     c
003900161202     c                   eval      OG129 = ORGdc9
004000161202    4c                   IF        §OGorfmd  > *zeros
004100161202     c                   eval      V02orfmd = %int(§OGorfmd)
004200161202
004300161202    5c                   if        tipo='P'
004400161202     c                   add       100           V02orfmd
004500161202    5c                   endif
004600161202    5c                   if        tipo='M'
004700161202     c                   sub       100           V02orfmd
004800161202    5c                   endif
004900161202
005000161202     c                   except    det
005100161202     c
005200161202     c* Aggazorg
005300161202     c                   if        %parms=2 and agg='AGG'
005400161202     c                   clear                   ORGFTR
005500161202     c                   movel     V02orfmd      §ogorfmd
005600161202     c                   eval      orgdc9= og129
005700161202     c                   except    aggazorg
005800161202     c                   endif
005900161202     c
006000161202     c
006100161202   x4c                   else
006200161202     c                   except    err
006300161202    4c                   ENDIF
006400161202    3c                   ENDIF
006500161202    3c                   ENDIF
006600161202
006700161202     c                   read      azorg01l
006800161202    2c                   enddo
006900161202
007000161202    1c                   ENDIF
007100161202     c
007200161202     c                   seton                                        lr
007300161202     c
007400021118      *________________________________________________
007500021118     oAZORG     e            aggAZORG
007600161202     o                       ORGDc9
007700161202     o                       ORGFTR
007800161202     OQSYSPRT   E            err         1
007900161202     O                                        +  20 'orari fil errati'
008000161202     o                       orgfil           +   1
008100161202     o                       orgdes           +   1
008200161202     OQSYSPRT   E            det         1
008300161202     O                                              'Fil'
008400161202     o                       orgfil           +   1
008500161202     o                       orgdes           +   1
008600161202     o                       V02orfmd         +   1
008700161202     OQSYSPRT   E            testa       1
008800161202     O                                              'Aggiorna orari FIL'
008900161202     o                       tipoop           +   3
009000161202     o                       tipoag           +   3
