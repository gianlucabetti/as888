000100020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200951009?     *--------------------------------------------------------------*
000300020411     D dataiso         s               d   datfmt(*iso)
000400161115      //********************************************************************************************
000500161115      //      Reperimento PEC
000600161115      //********************************************************************************************
000700161115
000800161115     d tibsipec1i    e ds                  qualified inz(*EXTDFT)
000900161115     d tibsipec1o    e ds                  qualified inz(*EXTDFT)
001000161115     d tibsipec2i    e ds                  qualified inz(*EXTDFT)
001100161115
001200161115      /COPY GAITRASRC/SRCPROTOPR,TIBSIPEC0
001300161115
001400161115     D prmRqsOpCode...
001500161115     D                 s             10I 0
001600161115     D prmRpyOpCode...
001700161115     D                 s             10I 0
001800161115     D prmRqsFormat...
001900161115     D                 s             10A
002000161115     D prmRqsData...
002100161115     D                 s          32767A   varying
002200161115     D prmRqsDataSize...
002300161115     D                 s             10I 0
002400161115     D prmRpyFormat...
002500161115     D                 s             10A
002600161115     D prmRpyData...
002700161115     D                 s          32767A
002800161115     D prmRpyDataSize...
002900161115     D                 s             10I 0
003000161115
003100161115      //********************************************************************************************
003200161115     C     *entry        plist
003300161115     C                   parm                    cfemi            20
003400161115     C                   parm                    pecEML          100
003500161115      **
003600161115     c                   exsr      Retrieve_PEC
003700161115      **
003800020529     C     FINE          TAG
003900951009     C                   SETON                                        LR
004000161115      *----------------------------------------------------------------------
004100161115      /FREE
004200161115
004300161115         BEGSR Retrieve_PEC;
004400161115
004500161115              tibsipec1i.cdfisc = cFEMI;
004600161115
004700161115             prmRqsOpCode = 0;
004800161115             prmRpyOpCode = 0;
004900161115             prmRqsFormat = 'TIBSIPEC1I';
005000161115             prmRqsData = tibsipec1i;
005100161115             prmRqsDataSize = %size(TIBSIPEC1I);
005200161115             prmRpyFormat = 'TIBSIPEC1O';
005300161115             prmRpyData =  ' ';
005400161115             prmRpyDataSize = %size(TIBSIPEC1O);
005500161115
005600161115         Get_Indirizzo_PEC(prmRqsOpCode:prmRpyOpCode:
005700161115               prmRqsFormat:prmRqsData:prmRqsDataSize:
005800161115               prmRpyFormat:prmRpyData:prmRpyDataSize);
005900161115
006000161115                 tibsipec1o = prmRpyData ;
006100161115
006200161115            if prmRpyOpCode = *zero and tibsipec1o.indpec <> *blank;
006300161115              PECEML  =  %trim(tibsipec1o.indpec);
006400161115            endif;
006500161115
006600161115         ENDSR;
006700161115
006800161115      /END-FREE
006900161115      *----------------------------------------------------------------------
