000100980506      *------------------------------------------------------------------------*
000200981009      * Gestione numeratore pratica assicurativa
000300980506      *------------------------------------------------------------------------*
000400980730     H DECEDIT('0,') DATEDIT(*DMY/)
000500981009      *----------------------------------
000600981009     fTNTBE01L  UF A E           K DISK
000700981009      *----------------------------------
000800981009     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
000900980914      *----------------------------------
001000981009     D FIDN20DS      E DS
001100980507     D KPJBA         E DS
001200981009     D DPAD          E DS
001300981009      *----------------------------------
001400981009     d                 DS                  INZ
001500981009     d  Kke1                               LIKE(TBEKE1)
001600981009     d  WI20dit                1      3
001700981027     d  WI20pra                4      7s 0
001800981009      *----------------------------------
001900981009     D Kcod            S                   LIKE(TBEcod)  INZ('PAD')
002000980731      *---------------------------------------------------------------------
002100981009     C     *ENTRY        PLIST
002200981009     C                   PARM                    KPJBA
002300981009
002400981009     c     KTBE          KLIST
002500981009     c                   KFLD                    Kcod
002600981009     c                   KFLD                    Kke1
002700981009      *---------------------------------------------------------------------
002800981001
002900981009     C                   MOVEL     KPJBU         FIDN20DS
003000981012     c                   clear                   dpad
003100981009
003200981009      * SE HO RICEVUTO SOCIETA' O ANNO NON VALIDI RESTITUISCO NUMERO = 0 E MESSAGGIO
003300981027     c                   IF        I20dit = *blanks  or  I20pra = *zeros
003400981012     c                             or (I20mod <> 'N'  and  I20mod <> 'V')
003500981009     C                   movel     'E'           O20err
003600981009     C                   movel(P)  MSG(1)        O20msg
003700981009     c                   ELSE
003800981009      *
003900981009     C                   MOVEL     I20dit        WI20dit
004000981027     C                   MOVEL     I20pra        WI20pra
004100981009
004200981009     c     Ktbe          chain     TNTBE000                           30
004300981008
004400981009     c                   IF        *IN30 = *ON
004500981012      *   SE LA TABELLA NON ESISTE LA CREO CON NUMERATORE 0 SE MODALITA' "VERIFICA" 1 SE "NUMERO"
004600981009     c                   clear                   TNTBE000
004700981009     C                   MOVEL     Kcod          TBEcod
004800981009     C                   MOVEL     Kke1          TBEke1
004900981012     c                   IF        I20mod = 'N'
005000981012     C                   add       1             §padnume
005100981012     c                   ENDIF
005200981009     C                   MOVEL(P)  DPAD          TBEuni
005300981009     C                   WRITE     TNTBE000
005400981009
005500981009     c                   ELSE
005600981009
005700981009     c                   clear                   dpad
005800981009      *   SE LA TABELLA E' ANNULLATA RESTITUISCO NUMERO = 0 E MESSAGGIO
005900981009     c                   IF        TBEatb <> *blanks
006000981009     C                   movel     'E'           O20err
006100981009     C                   movel(P)  MSG(1)        O20msg
006200981009     C                   ELSE
006300981012      *   SE LA TABELLA ESISTE ED E' RICHIESTO RECUPERO IL NUMERO
006400981012     c                   IF        I20mod = 'N'
006500981009     c                   movel     TBEuni        DPAD
006600981009     C                   add       1             §padnume
006700981009     C                   MOVEL(P)  DPAD          TBEuni
006800981009     C                   UPDATE    TNTBE000
006900981009     c                   ENDIF
007000981012     c                   ENDIF
007100981009      *
007200981009     c                   ENDIF
007300981009     c                   ENDIF
007400981009
007500981009      *   IMPOSTO I CAMPI PER IL RITORNO
007600981027     C                   z-add     §padnume      O20prn
007700981009     C                   MOVEL(P)  FIDN20DS      KPJBU
007800981009
007900980506     C                   EVAL      *INLR = *ON
008000981009      *----------------------------------------------------------------
008100981009** MSG                                                                       *
008200981009ERRORE NEI DATI RICEVUTI, NON E' POSSIBILE RICAVARE IL NUMERO PRATICA          1
008300981009LA TABELLA RISULTA ANNULATA, NON E' POSSIBILE RICAVARE IL NUMERO PRATICA       2
