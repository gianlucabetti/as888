000100110617      *---------------------------------------------------------------*
000200110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300130308     h dftactgrp(*no) actgrp(*caller)
000400110617
000500110617      *---------------------------------------------------------------*
000600130318
000700170125     D FNLSPDFR1       PR
000800170125     D                                     extpgm('FNLSPDFR1')
000900170125     D pRoot                        120A
001000170125     D pSubFlr                        7A
001100170125     D pMask                         61A
001200170125     D pCheckOnly                     1A
001300170125     D pFileName                    256A
001400170125     D pEsito                         1A
001500130309
001600130309
001700170125     D pRoot           s            120A
001800170125     D pSubFlr         s              7A
001900170125     D pMask           s             61A
002000170125     D pCheckOnly      s              1A
002100170125     D pFileName       s            256A
002200170125     D pEsito          s              1A
002300130309
002400110617
002500110627     C     *ENTRY        PLIST
002600161108     C                   PARM                    iMask            30
002700130309
002800110617      /free
002900130321
003000130321       // procedo con l'elaborazione
003100170125       pRoot   = '/QNTC/CEDFSBO1/VASPDF';
003200170125       pSubFlr = '1020001';
003300161108       pMask   = iMask;
003400161108       exsr sr_findFILE;
003500130401
003600130401       // chiusura *pgm
003700161108       eval *inlr = *on;
003800161108       return;
003900130401
004000130401
004100130401       //-------------------------------------------------------------*
004200161108       //Richiamo driver "trova file"                                 *
004300130401       //-------------------------------------------------------------*
004400161108        Begsr sr_findFILE;
004500130401
004600130416            pFileName  = *blanks;
004700161108            pCheckOnly = *blanks;
004800130416            pEsito     = '0';
004900130401
005000170125            callP(e) FNLSPDFR1 (pRoot      :
005100170125                                pSubFlr    :
005200170125                                pMask      :
005300170125                                pCheckOnly :
005400170125                                pFileName  :
005500170125                                pEsito);
005600130401
005700130401            // Verifico esito
005800130401            if %error;
005900130401               pEsito = '2';
006000130401            endif;
006100130401
006200170125            select;
006300170125            when pEsito = '0';
006400170125               dsply 'File Found';
006500170125            when pEsito = '2';
006600170125               dsply 'File NOT Found';
006700170125            when pEsito = '3';
006800170125               dsply 'More Files Found';
006900170125            other;
007000170125               dsply 'Undefined result';
007100170125            endsl;
007200130401
007300130401        endsr;
007400130321
007500130321
