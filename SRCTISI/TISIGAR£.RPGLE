000100970526     H*------------------------------------------------------------------------*
000200150210     H* "DRIVER" MOVIMENTI GABBIE E BANCALI ctr abilitazione cliente
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150211     FAZfgb02L  IF   E           K DISK
000900150210     FAZcGB01L  IF   E           K DISK
001000970526     D*------------------------------------------------------------------------*
001100970526     D* INPUT
001200970526     D*------------------------------------------------------------------------*
001300021218     D* DS "XSRDA8" - CONTROLLA DATA (8)
001400021218     D*------------
001500021218     D WLBDA8          DS                  INZ
001600021218     D  G08DAT                 1      8  0
001700021218     D  G08INV                 9     16  0
001800021218     D  G08ERR                17     17
001900021218     D  G08TGI                18     22  0
002000021218     D*------------
002100020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
002200021218     D*------------
002300150210     D TISIGaDS      E DS
002400021218     D*------------
002500021218     D* DS CHIAMAMTA *PGM REPERIMENTO DATI ALTRO UTENTE (TIBS36R)
002600021218     D*------------
002700021218     D TIBS36DS      E DS
002800021218     D*------------
002900021218     D* DS RIDEFINIZIONE CAMPO DATI UTENTE DELLA DS TIBS36DS RELATIVO ALLA STRUTTIRA DI AZUTE00F
003000021218     D*------------
003100021218     D AZUTE00F      E DS
003200021218     D*------------
003300021218     D* DS RIDEFINIZIONE CAMPO AUTORIZZAZIONI SPECIALI (UTEFAF) DI AZUTE00F
003400021218     D*------------
003500021218     D DUTE01        E DS
003600030122     D KPJBA         E DS
003700970526     C*------------------------------------------------------------------------*
003800970526     C* MAIN LINES
003900970526     C*------------------------------------------------------------------------*
004000150211     c                   exsr      defcam
004100970526     C*
004200150210     C                   IF        IGaTLA <> 'C'
004300020415     C                   EXSR      PROCEDI
004400020415     C                   ENDIF
004500020415     C*
004600970526     C* FINE PGM
004700150211     c                   exsr      srfine
004800150211     C*------------------------------------------------------------------------*
004900150211     C* srfine  -
005000150211     C*------------------------------------------------------------------------*
005100150211     C     srfine        BEGSR
005200150210     C                   IF        IGaTLA = 'E'
005300020415     C                   SETON                                        RT
005400020415     C                   ELSE
005500020415     C                   SETON                                        LR
005600020415     C                   ENDIF
005601150224     C                   if        OGaesito=' '
005700150224     c                   movel     cgbksu        ogaksu
005703150224     c                   end
005704150224     c                   movel     tisigads      kpjbu
005800150211     c                   return
005900150211     c*
006000150211     C                   ENDsr
006100970526     C*------------------------------------------------------------------------*
006200150210     C* PROCEDI - ELABORA richiesta
006300970526     C*------------------------------------------------------------------------*
006400020415     C     PROCEDI       BEGSR
006500021218     C* CONTROLLO ESISTENZA SOGGETTO MOVIMENTO
006600150211     C     igakscw       CHAIN     azfgb02l
006700150211     C                   IF        not %found(azfgb02l) or
006800150211     C                             (%found(azfgb02l) and fgbann <> ' ')
006900150211     C                   EVAL      OGaesito= '2'
007000150211     C                   EVAL      OGatesto= 'Cliente inesistente in anagrafico'
007100150210     C                   exsr      srfine
007200150210     c                   else
007300150210     c     kcgb          setgt     azcgb01l
007400150211     c     kcgb1         readpe    azcgb01l
007500150210     C                   IF        %eof(azcgb01l)
007600150211     C                   EVAL      OGaesito= '2'
007700150211     C                   EVAL      OGatesto='Cliente non abilitato al tipo gest+
007800150210     c                             ione'
007900150210     C                   exsr      srfine
008000021218     C                   ENDIF
008100150210     C                   ENDIF
008200150210     c* controllo la data
008300150210     c                   if        cgbdtf < igadat
008400150211     C                   EVAL      OGaesito= '2'
008500150211     C                   EVAL      OGatesto='Cliente non più abilitato alla ges+
008600150210     c                             tione'
008700150210     C                   exsr      srfine
008800150210     c                   end
008900150210     c* controllo l'articolo
009000150211     c                   if        igacod <> cgbcod1 and
009100150210     c                             igacod <> cgbcod2 and
009200150210     c                             igacod <> cgbcod3
009300150211     C                   EVAL      OGaesito='3'
009400150211     C                   EVAL      OGatesto='Cliente non abilitato al tipo banc+
009500150210     c                             ale'
009600150210     C                   exsr      srfine
009700150210     c                   end
009800021218     C*
009900021218     C                   ENDSR
010000970526     C*------------------------------------------------------------------------*
010100150211     C* OPERAZIONI INIZIALI
010200970526     C*------------------------------------------------------------------------*
010300150211     C     defcam        BEGSR
010400970526     C*---
010500970526     C* RICEVIMENTO PARAMETRI
010600970526     C*---
010700970526     C     *ENTRY        PLIST
010800150211     C                   PARM                    kpjba
010900150211     c                   movel     kpjbu         tisigads
011000150211     C* CALCOLA LA DATA CORRENTE
011100150211     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
011200150211     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
011300150211     C                   Z-ADD     WN8           G08DAT
011400150211     C                   Z-ADD     *ZEROS        G08INV
011500150211     C                   MOVEL     '0'           G08ERR
011600150211     C                   CALL      'XSRDA8'
011700150211     C                   PARM                    WLBDA8
011800150211     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
011900150211     c* data movimento
012000150211     c                   if        igadat = 0
012100150211     c                   eval      igadat =DATCOR
012200150211     c                   end
012300150211     c*
012400150211     c                   if        igatba <> 0 and igacod = ' '
012500150211     c                   move      igatba        igatbaw           2
012600150211     c                   eval      igacod = igatsc + igatbaw
012700150211     c                   end
012800150211     c* cliente di 8 alfa
012900150211     c                   move      *all'0'       igakscw           8
013000150211     c                   move      igaksc        igakscw           8
013100150211     C*
013200150224     C                   clear                   OGaksu
013201150224     C                   clear                   OGaesito
013300150211     C                   clear                   OGatesto
013400150210     C*
013500150210     C     Kcgb          KLIST
013600150211     C                   KFLD                    fgbksu                         *CODICE UTENTE
013700150210     C                   KFLD                    igatsc                         *CODICE CAPOCONTO
013800150210     C                   KFLD                    igadat                         *CODICE SOTTOCONTO
013900150210     C     Kcgb1         KLIST
014000150211     C                   KFLD                    fgbksu                         *CODICE UTENTE
014100150210     C                   KFLD                    igatsc                         *CODICE CAPOCONTO
014200970526     C*
014300970526     C                   ENDSR
