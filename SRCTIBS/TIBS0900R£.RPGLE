000100060303     ***********************************************************************************************
000200060303     **
000300060317     ** Questo modulo controlla se il conto ricevuto è "incassi da attribuire".
000400060303     **
000500060303     ***********************************************************************************************
000600060303     H NOMAIN
000700060303     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000800060303
000900060303     ***********************************************************************************************
001000060303     **
001100060303     ** Definizione files.
001200060303     **
001300060303     ***********************************************************************************************
001400060303     Ftntbe01l  IF   E           K DISK
001500060303     F                                     USROPN
001600060303
001700060303     ***********************************************************************************************
001800060303     **
001900060303     ** Definizione strutture dati.
002000060303     **
002100060303     ***********************************************************************************************
002200060303     D andiz00f      E DS
002300060303     D                                     BASED(nullPtr)
002400060303     D                                     PREFIX(pjz)
002500060303
002600060303     ***********************************************************************************************
002700060303     **
002800060303     ** Definizione variabili globali.
002900060303     **
003000060303     ***********************************************************************************************
003100060303     D nullPtr         S               *
003200060303     D conti           S             14A
003300060303     D                                     DIM(999)
003400060303     D                                     DESCEND
003500060303     D elementi        S              5I 0
003600060303
003700060303     D*--------------------------------------------------
003800060303     D* Procedure name: getContiIncassiAttribuire
003900060303     D* Purpose:        Restituisce tutti i sotto conti incassi da attribuire
004000060303     D* Returns:        Esito
004100060303     D* Parameter:      societa
004200060303     D* Parameter:      esito
004300060303     D* Parameter:      ksc
004400060303     D* Parameter:      elementi
004500060303     D*--------------------------------------------------
004600060303     D getContiIncassiAttribuire...
004700060303     D                 PR            10I 0
004800060303     D  societa                            LIKE(pjzSocieta)
004900060303     D                                     CONST
005000060303     D  esito                        10I 0
005100060303     D  conti                        14A
005200060303     D                                     DIM(999)
005300060303     D                                     DESCEND
005400060303     D                                     OPTIONS(*VARSIZE)
005500060303     D  elementi                      5I 0
005600060303
005700060303     D*--------------------------------------------------
005800060303     D* Procedure name: chkContoIncassiAttribuire
005900060303     D* Purpose:        Controlla se un conto è un conto "Incassi da attrib...
006000060303     D*                          uire".
006100060303     D* Returns:        Esito
006200060303     D* Parameter:      societa
006300060303     D* Parameter:      kcc => Capo conto
006400060303     D* Parameter:      ksc => Sotto conto
006500060303     D* Parameter:      esito
006600060303     D*--------------------------------------------------
006700060303     D chkContoIncassiAttribuire...
006800060303     D                 PR            10I 0
006900060303     D  societa                            LIKE(pjzSocieta)
007000060303     D  kcc                                LIKE(pjzKcc)
007100060303     D  ksc                                LIKE(pjzKsc)
007200060303     D  esito                        10I 0
007300060303
007400060303     P*--------------------------------------------------
007500060303     P* Procedure name: getContiIncassiAttribuire
007600060303     P* Purpose:        Restituisce tutti i sotto conti incassi da attribuire
007700060303     P* Returns:        Esito
007800060303     P* Parameter:      societa
007900060303     P* Parameter:      esito
008000060303     P* Parameter:      ksc
008100060303     P* Parameter:      elementi
008200060303     P*--------------------------------------------------
008300060303     P getContiIncassiAttribuire...
008400060303     P                 B                   EXPORT
008500060303     D getContiIncassiAttribuire...
008600060303     D                 PI            10I 0
008700060303     D  societa                            LIKE(pjzSocieta)
008800060303     D                                     CONST
008900060303     D  esito                        10I 0
009000060303     D  conti                        14A
009100060303     D                                     DIM(999)
009200060303     D                                     DESCEND
009300060303     D                                     OPTIONS(*VARSIZE)
009400060303     D  elementi                      5I 0
009500060303
009600060303     D dy4o          E DS
009700060303     D                                     INZ
009800060303     D i               S              5I 0
009900060303
010000060303      /FREE
010100060303
010200060303       CLEAR esito;
010300060303       CLEAR conti;
010400060303       CLEAR elementi;
010500060303
010600060303       OPEN tntbe01l;
010700060303       SETLL ('Y4O':societa) tntbe01l;
010800060303
010900060303       DOU %EOF;
011000060303         READE ('Y4O':societa) tntbe01l;
011100060303         IF %EOF;
011200060303           LEAVE;
011300060303         ENDIF;
011400060303         IF tbeAtb <> ' ';
011500060303           ITER;
011600060303         ENDIF;
011700060303         dy4o = tbeUni;
011800060303         IF §4oSc1 <> *ZERO AND §4oCc1 <> *ZERO;
011900060303           elementi += 1;
012000060303           conti(elementi) = §4oCc1 + §4oSc1;
012100060303         ENDIF;
012200060303       ENDDO;
012300060303
012400060303       CLOSE tntbe01l;
012500060303       SORTA conti;
012600060303
012700060303       RETURN esito;
012800060303
012900060303      /END-FREE
013000060303     P getContiIncassiAttribuire...
013100060303     P                 E
013200060303
013300060303     P*--------------------------------------------------
013400060303     P* Procedure name: chkContoIncassiAttribuire
013500060303     P* Purpose:        Controlla se un conto è un conto "Incassi da attrib...
013600060303     P*                          uire".
013700060303     P* Returns:        Esito
013800060303     P* Parameter:      societa
013900060303     P* Parameter:      kcc => Capo conto
014000060303     P* Parameter:      ksc => Sotto conto
014100060303     P* Parameter:      esito
014200060303     P*--------------------------------------------------
014300060303     P chkContoIncassiAttribuire...
014400060303     P                 B                   EXPORT
014500060303     D chkContoIncassiAttribuire...
014600060303     D                 PI            10I 0
014700060303     D  societa                            LIKE(pjzSocieta)
014800060303     D  kcc                                LIKE(pjzKcc)
014900060303     D  ksc                                LIKE(pjzKsc)
015000060303     D  esito                        10I 0
015100060303
015200060303     D getContiIncassiAttribuire...
015300060303     D                 PR            10I 0
015400060303     D  societa                            LIKE(pjzSocieta)
015500060303     D                                     CONST
015600060303     D  esito                        10I 0
015700060303     D  conti                        14A
015800060303     D                                     DIM(999)
015900060303     D                                     DESCEND
016000060303     D                                     OPTIONS(*VARSIZE)
016100060303     D  elementi                      5I 0
016200060303
016300060303     D conto           S             14A
016400060303     D i               S              5I 0
016500060303
016600060303      /FREE
016700060303
016800060303       CLEAR esito;
016900060303
017000060303       IF societa = ' ' OR kcc = ' ' OR ksc = ' ';
017100060317         esito = -1;
017200060317         RETURN esito;
017300060303       ENDIF;
017400060303
017500060303       IF elementi = 0;
017600060303         getContiIncassiAttribuire(societa:esito:conti:elementi);
017700060303         IF esito <> 0;
017800060317           esito = -2;
017900060317           RETURN esito;
018000060303         ENDIF;
018100060303       ENDIF;
018200060303
018300060303       conto = kcc + ksc;
018400060303       IF %LOOKUP(conto:conti:1:elementi) > 0;
018500060317         esito = 1;
018600060317         RETURN esito;
018700060303       ENDIF;
018800060303
018900060303       RETURN esito;
019000060303
019100060303      /END-FREE
019200060303     P chkContoIncassiAttribuire...
019300060303     P                 E
019400060303
