package com.wallit.rascalchat;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;

public class Login extends Activity {
	private EditText etUsername;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.login);
		
		etUsername = (EditText) findViewById(R.id.username);
	}
	
	public void loginClicked(View view) {
		String username = etUsername.getText().toString().trim();
		if (username.length() <= 0) {
			// case: empty username
			return; 
		}
		
		Intent intent = new Intent(this, Chat.class);
		intent.putExtra(Chat.IE_USERNAME, username); 
		startActivity(intent); 
	} 
}
