000100060317     H NOMAIN
000200060317
000300060331     D*--------------------------------------------------
000400060331     D* Procedure name: getIdSessione
000500060331     D* Purpose:        Restituisce un ID sessione.
000600060331     D* Returns:        ID sessione
000700060331     D*--------------------------------------------------
000800060331     D getIdSessione   PR            15P 0
000900060331
001000060331     P*--------------------------------------------------
001100060331     P* Procedure name: getIdSessione
001200060331     P* Purpose:        Restituisce un ID sessione.
001300060331     P* Returns:        ID sessione
001400060331     P*--------------------------------------------------
001500060331     P getIdSessione   B                   EXPORT
001600060331     D getIdSessione   PI            15P 0
001700060331
001800060331     D now             S               Z
001900060331     D idSessione      S             15P 0
002000060331
002100060331      /FREE
002200060331       CLEAR idSessione;
002300060331       now = %TIMESTAMP();
002400060331       idSessione = (UYEAR * 10000000000000)
002500060331       + ((%SUBDT(now:*M)) * 100000000000) + ((%SUBDT(now:*D)) * 1000000000)
002600060331       + ((%SUBDT(now:*H)) * 10000000) + ((%SUBDT(now:*MN)) * 100000)
002700060331       + ((%SUBDT(now:*S)) * 1000) + (%SUBDT(now:*MS) / 1000);
002800060331       RETURN idSessione;
002900060331      /END-FREE
003000060331
003100060331     P getIdSessione   E
