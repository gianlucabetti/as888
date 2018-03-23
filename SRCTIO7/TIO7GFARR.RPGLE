000100060623     H NOMAIN BNDDIR('TIBS')
000200030616
000300030616     D GetDescrizioneFase...
000400030616     D                 PR            35A
000500030616     D ParametroI                     3P 0
000600030616     D                                     VALUE
000700030616     D ParametroO                     1
000800030616     D ParametroO
000900060623     D                                     LIKE(t02Uni)
001000030616     D                                     OPTIONS(*OMIT)
001100060623     D langISO2                       2A   OPTIONS(*NOPASS)
001200060623
001300060623     D cvtLinguaISO2ToTntbe...                                                  Conversione lingua
001400060623     D                 PR                  LIKE(t02Lin)
001500060623     D  rqsISO2                       2A
001600060623     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
001700060623     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
001800060623     D  rpyTntbe                           LIKE(t02Lin)
001900060623     D                                     OPTIONS(*NOPASS:*OMIT)
002000060623
002100060623     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
002200060623     D  kpjba                              LIKEDS(kpjba)
002300060623     D  tibs02ds                           LIKEDS(tibs02ds)
002400060623
002500060623     D kpjba         E DS                  INZ
002600060623     D tibs02ds      E DS                  INZ
002700060623     D  t02Mod       E                     INZ('C')                             Controllo
002800060623     D  t02Cod       E                     INZ('FAR')
002900060623
003000030616     P GetDescrizioneFase...
003100030616     P                 B                   EXPORT
003200030616
003300030616     D GetDescrizioneFase...
003400030616     D                 PI            35A
003500030616     D Fase                           3P 0
003600030616     D                                     VALUE
003700030616     D Esito                          1
003800030616     D Dati
003900060623     D                                     LIKE(t02Uni)
004000030616     D                                     OPTIONS(*OMIT)
004100060623     D langISO2                       2A   OPTIONS(*NOPASS)
004200030616
004300030616     D Eseguito        C                   '0'
004400030616     D Errore          C                   '1'
004500030616     D Descrizione     S
004600030616     D                                     LIKE(D§FARDes)
004700030616     D DFAR          E DS
004800030616
004900060623     C                   RESET                   tibs02ds
005000060623     C                   IF        %PARMS > 3
005100060623     C                   EVAL      t02Lin = cvtLinguaISO2ToTntbe(langISO2)
005200060623     C                   ENDIF
005300060623     C                   EVAL      t02Ke1 = %EDITC(fase:'X')
005400060623     C                   CALLP     tibs02r(kpjba:tibs02ds)
005500060623     C                   IF        t02Err = 'E'
005600030616     C                   CLEAR                   DFAR
005700030616     C                   CLEAR                   Descrizione
005800030616     C                   EVAL      Esito = Errore
005900030617     C                   IF        %ADDR(Dati) <> *NULL
006000030617     C                   CLEAR                   Dati
006100030617     C                   ENDIF
006200030616     C                   ELSE
006300060623     C                   EVAL      DFAR = t02Uni
006400030616     C                   IF        D§FARDVA <> *BLANK
006500030616     C                   EVAL      Descrizione = D§FARDVA
006600030616     C                   ELSE
006700030616     C                   EVAL      Descrizione = D§FARDes
006800030616     C                   ENDIF
006900030616
007000160318     ** La fase non deve essere pubblicata sul sito.
007100160318     C                   IF        d§farUtt <> 'S'
007200030616     C                   CLEAR                   Descrizione
007300030616     C                   ENDIF
007400030616
007500030616     C                   EVAL      Esito = Eseguito
007600030616
007700030616     C                   ENDIF
007800030616
007900030617     C                   IF        %ADDR(Dati) <> *NULL
008000030616     C                   EVAL      Dati = DFAR
008100030617     C                   ENDIF
008200030616
008300030616     C                   RETURN    Descrizione
008400030616
008500030616     P GetDescrizioneFase...
008600030616     P                 E
