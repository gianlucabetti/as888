000100090810     C*
000200170530     C* A seconda del Tipo ID Spedizione richiesto in input determino la "chiave"
000300170530     C                   movel     *blanks       wMaskTIS         64
000400170530     C                   select
000500171012     C*
000600171012     C                   when      iS716TIS = 'K'
000700171012     C                   eval      wMaskTIS = %editc(s716_NSP:'X') +
000800171012     C                                        %editc(s716_RMN:'X') + s716_RSD
000900170530     C*
001000170530     C                   when      iS716TIS = 'S'
001100170530     C                   eval      wMaskTIS = %editc(s716_NSP:'X')
001200170530     C*
001300170530     C                   when      iS716TIS = 'N'
001400170530     C                   eval      wMaskTIS = %editc(s716_RMN:'X') + s716_RSD
001500170530     C*
001600170530     C                   when      iS716TIS = 'A'
001700170530     C                   eval      wMaskTIS = s716_RMA + s716_RSD
001800170530     C*
001900170530     C                   when      iS716TIS = '3'
002000170530     C                   eval      wMaskTIS = %editc(s716_RMN:'X') + s716_RMA
002100170530     C*
002200170530     C                   other
002300170530     C                   eval      wMaskTIS = *all'*'
002400170530     C                   endsl
002500170530     C*
002600100330
