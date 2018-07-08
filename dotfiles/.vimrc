"Function that sends the current buffer to XJet python"
function! XJetPython()

py3 <<__
import vim

from struct import pack, unpack
import socket

# Remote damon commands
# Get current seqeuncer state
GET_PRINTER_NAME_CMD = 0

# Get online/offline status
GET_ONLINE_STATUS_CMD = 1

# Get jobs list
GET_JOBS_CMD = 2

# Get current seqeuncer state
GET_SEQ_STATE_CMD = 3

# Set online/offline status
# Data: U32 Online = 1, Offline =0
SET_ONLINE_STATUS_CMD = 4

# Stop current sequence
SEQ_STOP_CMD = 5

# Pause current sequence
SEQ_PAUSE_CMD = 6

# Resume current sequence
SEQ_RESUME_CMD = 7

# Delete job
# Data: U32 JobID for delete
JOB_DELETE_CMD = 8

# Delete all jobs
JOB_DELETE_ALL_CMD = 9

# Dump trace and abort
DUMP_TRACE_AND_ABORT_CMD = 10

# Application watchdog 
APP_WATCHDOG_CMD = 11

EVAL_PYTHON_STRING = 12
EVAL_PYTHON_FILE = 13
GET_PYTHON = 14

# Replies from printer to host

# Ack/Nak messsages (no data)
ACK_REPLY = 0
NAK_REPLY = 1

# Printer name reply
PRINTER_NAME_REPLY = 2

# Online/offline status reply
# Data: U32 Online = 1, Offline =0
ONLINE_STATUS_REPLY = 3

# Jobs list reply
# Data: TPE_JobInfo * JobsNum
JOBS_LIST_REPLY = 4

# Seqeuncer state reply
# Data: U32 Seqeuncer status (as defined in PETypes.h")
RA_SEQ_STATE_REPLY = 5

PYTHON_REPLY = 6

class RemoteClient:
  def __init__(self,
               Host = '127.0.0.1',
               Port = 2000):
    self.Host = Host
    self.Port = Port
  
  def GetReply(self, Socket, MessageSize=64):
    Data = Socket.recv(12)
    # Decode the data
    Signature = unpack('I',Data[0:4])[0]
    assert(Signature == 0x11223344)
    OutOpCode = unpack('I', Data[4:8])[0]
    MessageLen = unpack('I', Data[8:12])[0]
    Message = Socket.recv(MessageLen)
    return OutOpCode, Message
  
  def GetAck(self, Socket):
    OutOpCode,Message = self.GetReply(Socket)
    assert OutOpCode==0, "OpCode"
    assert Message == b'', "Message"

  def SendMessage(self, OpCode, Data=''):
    '''Send a message and return the reply as a Op,Message tuple'''
    Message = b'\x44\x33\x22\x11' + pack('I',OpCode) + pack('I',len(Data)) + bytes(Data,encoding='utf-8')
    Socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    Socket.connect((self.Host, self.Port))
    Socket.send(Message)

    # Get the response
    self.GetAck(Socket)
    OpCode,Reply = self.GetReply(Socket)

    Socket.close()
    return OpCode, str(Reply,encoding='utf-8')

  def EvalPythonString(self,Code):
    RetOp,Res = rc.SendMessage(EVAL_PYTHON_STRING, Code)
    if RetOp==1:
      raise Exception(Res)

  def RunPythonFile(self,Code):
    RetOp,Res = rc.SendMessage(EVAL_PYTHON_FILE, Code)
    if RetOp==1:
      raise Exception(Res)

  def GetPython(self,Expr):
    RetOp,Res = rc.SendMessage(GET_PYTHON, Expr)
    if RetOp==1:
      raise Exception(Res)
    return Res

if vim.current.buffer.options['modified']:
  vim.command("w")   # Write the current file
rc = RemoteClient()
try:
  rc.RunPythonFile(vim.current.buffer.name)
except Exception as e:
  print(str(e))
__
endfunc

"Map to F5"
map <F5> :call XJetPython()<CR>
