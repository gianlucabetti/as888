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
005700150224     C                   if        OGaesito=' '
005800150224     c                   movel     cgbksu        ogaksu
005900150224     c                   end
006000150224     c                   movel     tisigads      kpjbu
006100150211     c                   return
006200150211     c*
006300150211     C                   ENDsr
006400970526     C*------------------------------------------------------------------------*
006500150210     C* PROCEDI - ELABORA richiesta
006600970526     C*------------------------------------------------------------------------*
006700020415     C     PROCEDI       BEGSR
006800021218     C* CONTROLLO ESISTENZA SOGGETTO MOVIMENTO
006900160610     C     igakscw       setll     azfgb02l
007000160610     c                   do        *hival
007100160610     C     igakscw       reade     azfgb02l
007200160610     C                   IF        %eof(azfgb02l)
007300160610     C                   EVAL      OGaesito= '2'
007400160610     C                   EVAL      OGatesto= 'Cliente inesistente in anagrafico'
007500160610     c                   leave
007600160610     c                   endif
007700160610     C                   if        fgbann = ' '
007800160610     c     kcgb          setgt     azcgb01l
007900160610     c     kcgb1         readpe    azcgb01l
008000160610     C                   IF        %eof(azcgb01l)
008100160610     C                   EVAL      OGaesito= '2'
008200160610     C                   EVAL      OGatesto='Cliente non abilitato al tipo gest+
008300160610     c                             ione'
008400160610     c                   else
008500160610     c                   leave
008600160610     C                   ENDIF
008700150210     C                   ENDIF
008800160610     C                   enddo
008900150210     c* controllo la data
009000150210     c                   if        cgbdtf < igadat
009100150211     C                   EVAL      OGaesito= '2'
009200150211     C                   EVAL      OGatesto='Cliente non più abilitato alla ges+
009300150210     c                             tione'
009400150210     C                   exsr      srfine
009500150210     c                   end
009600150210     c* controllo l'articolo
009700150211     c                   if        igacod <> cgbcod1 and
009800150210     c                             igacod <> cgbcod2 and
009900150210     c                             igacod <> cgbcod3
010000150211     C                   EVAL      OGaesito='3'
010100150211     C                   EVAL      OGatesto='Cliente non abilitato al tipo banc+
010200150210     c                             ale'
010300150210     C                   exsr      srfine
010400150210     c                   end
010500021218     C*
010600021218     C                   ENDSR
010700970526     C*------------------------------------------------------------------------*
010800150211     C* OPERAZIONI INIZIALI
010900970526     C*------------------------------------------------------------------------*
011000150211     C     defcam        BEGSR
011100970526     C*---
011200970526     C* RICEVIMENTO PARAMETRI
011300970526     C*---
011400970526     C     *ENTRY        PLIST
011500150211     C                   PARM                    kpjba
011600150211     c                   movel     kpjbu         tisigads
011700150211     C* CALCOLA LA DATA CORRENTE
011800150211     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
011900150211     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
012000150211     C                   Z-ADD     WN8           G08DAT
012100150211     C                   Z-ADD     *ZEROS        G08INV
012200150211     C                   MOVEL     '0'           G08ERR
012300150211     C                   CALL      'XSRDA8'
012400150211     C                   PARM                    WLBDA8
012500150211     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
012600150211     c* data movimento
012700150211     c                   if        igadat = 0
012800150211     c                   eval      igadat =DATCOR
012900150211     c                   end
013000150211     c*
013100150211     c                   if        igatba <> 0 and igacod = ' '
013200150211     c                   move      igatba        igatbaw           2
013300150211     c                   eval      igacod = igatsc + igatbaw
013400150211     c                   end
013500150211     c* cliente di 8 alfa
013600150211     c                   move      *all'0'       igakscw           8
013700150211     c                   move      igaksc        igakscw           8
013800150211     C*
013900150224     C                   clear                   OGaksu
014000150224     C                   clear                   OGaesito
014100150211     C                   clear                   OGatesto
014200150210     C*
014300150210     C     Kcgb          KLIST
014400150211     C                   KFLD                    fgbksu                         *CODICE UTENTE
014500150210     C                   KFLD                    igatsc                         *CODICE CAPOCONTO
014600150210     C                   KFLD                    igadat                         *CODICE SOTTOCONTO
014700150210     C     Kcgb1         KLIST
014800150211     C                   KFLD                    fgbksu                         *CODICE UTENTE
014900150210     C                   KFLD                    igatsc                         *CODICE CAPOCONTO
015000970526     C*
015100970526     C                   ENDSR
