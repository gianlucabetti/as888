000100060908     H NOMAIN
000200060908
000300060908     D*--------------------------------------------------
000400060908     D* Procedure name: now
000500060908     D* Purpose:        Restituisce un timestamp completo.
000600060908     D* Returns:
000700060908     D*--------------------------------------------------
000800060908     D now             PR              Z
000900060908
001000060908     P*--------------------------------------------------
001100060908     P* Procedure name: now
001200060908     P* Purpose:        Restituisce un timestamp completo.
001300060908     P* Returns:
001400060908     P*--------------------------------------------------
001500060908     P now             B                   EXPORT
001600060908     D now             PI              Z
001700060908
001800060908     D retField        S               Z
001900060908
002000060908     C/EXEC SQL
002100060908     C+ SET :retField = NOW()
002200060908     C/END-EXEC
002300060908     C                   RETURN    retField
002400060908
002500060908     P now             E
