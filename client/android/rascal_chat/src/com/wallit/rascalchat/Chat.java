package com.wallit.rascalchat;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.util.List;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

public class Chat extends Activity {
	public static final String IE_USERNAME = "username";
	
	private static final int PORT = 20000;
	private static final byte[] IP_BYTES = {10, 0, 2, 2};
	private static final String IP = "10.0.2.2";
	private static final String HOST = "wayne.wallitt.com";
	
	EditText input;
	ListView messageLog;
	TextView tvUsername;

	Network network;
	Thread networkThread;
	MessageLogAdapter adapter;
	
	String username;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        
        messageLog = (ListView) findViewById(R.id.listView1);
        input = (EditText) findViewById(R.id.editText1);
        tvUsername = (TextView) findViewById(R.id.username);
        
        messageLog.setTranscriptMode(ListView.TRANSCRIPT_MODE_ALWAYS_SCROLL);
        messageLog.setStackFromBottom(true);
        
        adapter = new MessageLogAdapter(this, R.layout.message_list_item); 
        messageLog.setAdapter(adapter);
        
        username = getIntent().getStringExtra(IE_USERNAME); 
        tvUsername.setText(username);
    }
    
    @Override
    protected void onResume() {
    	super.onResume();
    	
        network = new Network();
        networkThread = new Thread(network);
        networkThread.start();
    }
    
    @Override
    protected void onPause() {
    	super.onPause();
    	network.close();
    	networkThread = null;
    	network = null;
    }
    
    public void sendClicked(View view) {
    	String message = input.getText().toString();
    	message = message.trim();
    	if (message.length() > 0) {
	    	network.sendMessage(message);
    	}
    	input.setText("");
    }
    
    class MessageLogAdapter extends ArrayAdapter<String[]> {
    	private int textViewResourceId;
    	private LayoutInflater inflater;
    	
		public MessageLogAdapter(Context context, int textViewResourceId) {
			super(context, textViewResourceId);
			this.textViewResourceId = textViewResourceId;
		}
		
		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			String[] message = getItem(position);
			String sender = message[0];
			String messageBody = message[1];
			
			if (convertView == null) {
				convertView = getLayoutInflater().inflate(textViewResourceId, 
						parent, false);
			}
			
			TextView send, body;
			send = (TextView) convertView.findViewById(R.id.sender);
			body = (TextView) convertView.findViewById(R.id.body);
			send.setText(sender);
			body.setText(messageBody);
			
			return convertView; 
		}
    	
    }
    
    class Network implements Runnable {
    	Socket socket = null;
    	DataInputStream is = null;
    	DataOutputStream os = null;
    	String username = null;
    	Handler handler;
    	boolean closed;
    	
    	public Network() {
    		this.handler = new Handler();
    	}
    	
		@Override
		public void run() {
			closed = false;
			try {
				socket = new Socket(HOST, PORT);
				is = new DataInputStream(socket.getInputStream());
				os = new DataOutputStream(socket.getOutputStream());
			} catch (UnknownHostException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			
			connect(Chat.this.username);
			
			if (socket != null && is != null && os!= null) {
				while (true) {
					byte[] buf = new byte[1024];
					int bytesRead = 0;
					try {
						bytesRead = is.read(buf);
					} catch (SocketTimeoutException e) {
						// case: timeout has occurred
						if (closed) {
							onClose();
							return;
						} else {
							continue;
						}
					} catch (IOException e) {
						e.printStackTrace();
					}
					String message = new String(buf, 0, bytesRead);
					System.out.format("MESSAGE! ::: %s/n", message);
					final String[] frags = message.split(";", 0);
					
					handler.post(new Runnable() {
						@Override
						public void run() {
							adapter.add(frags);
						}
					}); 
				}
			}
		} 
		
		public synchronized void sendMessage(String message) {
			StringBuilder builder = new StringBuilder();
			builder.append("send").append(";").append(message);
			String command = builder.toString();
			try {
				os.write(command.getBytes("US-ASCII"));
				os.flush();
			} catch (IOException e) {
				e.printStackTrace();
			}
			
			final String[] frags = new String[2];
			frags[0] = username;
			frags[1] = message;
			
			handler.post(new Runnable() {
				@Override
				public void run() {
					adapter.add(frags); 
				}
			}); 
		}
		
		public synchronized void connect(String username) {
			this.username = username;
			StringBuilder sb = new StringBuilder();
			String message = sb.append("register").append(";").append(username).toString();
			
			try {
				os.write(message.getBytes("US-ASCII"));
				os.flush();
			} catch (IOException e) {
				e.printStackTrace();
			}
			
			byte[] buf = new byte[1024];
			String msg = null;
			try {
				int readLen = is.read(buf);
				if (readLen > 0) {
					msg = new String(buf, 0, readLen);
				}
				socket.setSoTimeout(50);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		public synchronized void close() {
			closed = true;
		}
		
		private synchronized void onClose() {
			try {
				is.close();
				os.close();
				socket.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		private byte[] string2bytes(String string) {
			byte[] ret = null; 
			
			try {
				ret = string.getBytes("US-ASCII");
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
			
			return ret; 
		}
    }
}