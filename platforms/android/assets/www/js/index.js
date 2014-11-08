/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
var app = {
    // Application Constructor
    initialize: function() {
        this.bindEvents();
    },
    // Bind Event Listeners
    //
    // Bind any events that are required on startup. Common events are:
    // 'load', 'deviceready', 'offline', and 'online'.
    bindEvents: function() {
        document.addEventListener('deviceready', this.onDeviceReady, false);
    },
    // deviceready Event Handler
    //
    // The scope of 'this' is the event. In order to call the 'receivedEvent'
    // function, we must explicitly call 'app.receivedEvent(...);'
    onDeviceReady: function() {
        app.receivedEvent('deviceready');
        
    },
    // Update DOM on a Received Event
    receivedEvent: function(id) {
    
    $( document ).on( "pagecreate", "#alists", function( event ) {
    console.log( "This page was just enhanced by jQuery Mobile!" );
	});
    	console.log("in receivedEvent");
        
        var inthislist = null;
        
        var newListName = null;
        
        
        
        db.transaction(queryDB,errorCB);
        
        function queryDB(tx){
        tx.executeSql("SELECT name FROM sqlite_master WHERE type='table'",[],querySuccess,errorCB);
        }
        function querySuccess(tx,result){
        var len = result.rows.length;
		$('#listing').empty();
		    for(var i=1;i<len;i++){
                
                $('#listing').append('<li id="items"><a href="#listitems"><h3 class="ui-li-heading">'+result.rows.item(i).name+'</h3></a></li>');
      
            } 
		      
		 
		        $('#listing').listview();
        }
        function errorCB(err){
        	$('#listing').append('<li id="items"><a href="#"><h3 class="ui-li-heading">'+err+'</h3></a></li>');
        }
        
        
        $('#listnamesubmit').click(function(){
        newListName = $('#listname').val();
        console.log('listname',newListName);
        table_list = newListName;
        inthislist=newListName;
        createTable(table_list,listFields,{"id":"primary key","item":"not null","list":"not null","bought":"not null"});
        $("#namehead").append(inthislist);
        });
        
        $('#itemadd').click(function(){
        var itemadded = $('.itemname').val();
        $('#itemslisting').append('<div class="item">' +itemID+'.'+itemadded + '</div>' );
        $('.itemname').val("");
        createPara(itemID,itemadded,newListName);
        itemID+=1;
        insertTable(table_list,listFields,insertP);
        });
        
        $('#listing').live(function(){
        console.log('yo');
        inthislist = $(this).html();
        console.log(inthislist);
        });
        
        
        
        
        
        

        console.log('Received Event: ' + id);
    }
};

app.initialize();